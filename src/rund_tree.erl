%%%-------------------------------------------------------------------
%% @doc runddb public API
%% @end
%%%-------------------------------------------------------------------

-module(rund_tree).

-behaviour(gen_server).

-export([get_tree/2, get_bulls/0 ]).

-export([init/1, start_link/0, new/5, test/2, update/2 ] ).

-export([ handle_call/3, handle_cast/2 ] ).

-record( ftree, { id, ext_id, father, mother, birthdate, data } ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  User functions...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Get information about Id, for N generations(including Id)
get_tree( Id = { _Type, _Id}, N ) ->
	gen_server:call( rund_tree, {get_gen, Id, N } );
get_tree( Id, N ) -> 
	get_tree( rund_util:decode( Id ), N ).

get_bulls() ->
	gen_server:call( rund_tree, get_bulls ).

update( Id, Data ) ->
	gen_server:call( rund_tree, {update, Data} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   Genserver stuff...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init( _ ) -> 
	% create table if not present
	case mnesia:create_table( ftree, 
				  [ 
				   {attributes, record_info( fields, ftree ) }, 
				   {disc_copies, [ node() ] }, 
				   {index, [ext_id]} 
				  ] 
				) of
		{aborted, {node_not_running, _}} -> 
			rund_init:start_db(),
			init(x);
		{aborted,{already_exists,ftree}} -> { ok, [] };
		{atomic, ok } -> rund_init:db(),
				 { ok, [] }
	end.

start_link() ->
	gen_server:start_link( { local, rund_tree },
			       ?MODULE,
			       "",
			       [] ).

handle_call( {get_gen, Id, N}, _From, State ) ->
	{atomic, Data } = mnesia:transaction(
			    fun () ->
					    get_gen( Id, N )
			    end
			   ),
	{ reply, Data, State };
handle_call( get_bulls, _From, State ) ->
	{atomic,Data } = mnesia:transaction(
			   fun () ->
					   mnesia:match_object( 
					     #ftree{ _ = '_' } )
			   end ),
	Rv = lists:filtermap( 
	       fun ( X ) -> 
			       case format_ftree(X) of
				       Rv = #{ sex := m } -> { true, Rv };
				       _	-> false
			       end
	       end, 
	       Data ),
	{ reply, Rv, State }.

handle_cast( { update_parents, Id, [ExtId|R] }, State ) ->
	ok = gen_server:cast( self(), { update_parents, Id, ExtId } ),
	case R of
		[]	-> ok;
		_	-> ok = gen_server:cast( self(), { update_parents, Id, R } )
	end,
	{noreply, State };
handle_cast( { update_parents, Id, ExtId }, State ) ->
	{ atomic, ok } = mnesia:transaction( 
			   fun () -> 
					   ok = update_parent( mother, ExtId, Id ),
					   ok = update_parent( father, ExtId, Id )
			   end ),
	{ noreply, State };	
handle_cast( Req, State ) -> 
	io:format("Hi...~p~n", [Req] ), 
	{ noreply, State }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Database operations
%%%  	insert into transaction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

id_scan( Data, T ) ->
	case Data of
		#{ T := Id } -> { #{ T => Id }, maps:remove( T, Data ) };
		_	-> { #{}, Data } 
	end.

%%% Insert record
%%% 	Id's of parents will be translated to internal in-keys. Unless it's
%%% 	the initial run. In the initial run, the parent id's are translated 
%%% 	later, in one run.
new( { Type, Id }, Father, Mother, Birthdate, Unsorted ) ->
	NewId = rund_util:uniq_id(ftree),
	{ Ext_id, Data } = id_scan( Unsorted, foknummer ),
	X = #ftree{ id = NewId,
			     ext_id = Ext_id#{ Type => Id },
			     father = Father,
			     mother = Mother,
			     birthdate = Birthdate,
			     data = Data },
	ok = mnesia:write(X),
	ok = gen_server:cast( rund_tree, { update_parents, 
				      NewId, 
				      maps:to_list(Ext_id) } ),
	NewId.

format_ftree( #ftree{ ext_id = Eid, birthdate = Bd, data = Data } ) ->
	X = maps:merge( Eid, Data ),
	X#{ geboortedatum => Bd };
format_ftree( X ) -> 
	io:format( "~p~n", [X] ),
	unknown.

get_ftree( EId = { _Type, _Id } ) ->
	get_ftree( norm_id(EId) );
get_ftree( unknown ) -> unknown;
get_ftree( Id ) ->
	case mnesia:read( ftree, Id ) of
		[X] -> X;
		X -> io:format("Not found: ~p -> ~p~n", [ Id, X ] )
	end.

get_gen( unknown, _ ) -> unknown;
get_gen( {_,unknown}, _ ) -> unknown;
get_gen( Id, 0 ) ->
	io:format( "~p~n", [Id] ),
	rund_util:encode( norm_id( Id ) );
% Oldest in the tree gets the parents
get_gen( Id, N ) ->
	case get_ftree( Id ) of
		Child = #ftree{ father = F, mother = M } ->
			R1 = format_ftree( Child ),
			R1#{ vader => get_gen( F, N-1 ),
			   moeder => get_gen( M, N-1 )};
		unknown -> unknown
	end.
	
%% internal functions

norm_id( { _IdType, unknown } ) -> unknown;
norm_id( { IdType, IdValue } ) ->
	case mnesia:select( ftree, 
			    [
			     { #ftree{ id = '$1', ext_id = '$2', _='_' }, 
			       [ { '==', 
				   IdValue,
				   { map_get, IdType, '$2'  } }
			       ],
			       [ '$1' ]
			     }
			    ]
			     ) of
		[]	-> { IdType, IdValue };
		[ Id ] ->	Id;
		X	-> io:format("~p~n", [X]),
			   mnesia:abort( ftree_ext_id_duplicate )
	end;
norm_id( Id ) -> Id.

update_ftree_parent( X = #ftree{}, father, Id ) ->
	X#ftree{ father = Id };
update_ftree_parent( X = #ftree{}, mother, Id ) ->
	X#ftree{ mother = Id }.

update_parent( Type, ExtId, Id ) ->
	Match = update_ftree_parent( #ftree{ _ = '_' }, Type, ExtId ),
	Childs = mnesia:match_object( ftree, Match, write ),
	lists:foreach( fun ( Rec = #ftree{} ) ->
				       mnesia:write( 
					 update_ftree_parent( Rec, Type, Id )
					)
		       end, Childs ).


test( Id, N ) -> mnesia:transaction(
		   fun () ->
				   get_gen( Id, N )
		   end
		  ).
			
