%%%-------------------------------------------------------------------
%% @doc runddb public API
%% @end
%%%-------------------------------------------------------------------

-module(rund_init).


-export([ db/0, start_db/0 ]).


db() -> 
	  { atomic, ok } = rund_util:init(),
	  { ok, F } = file:list_dir( "priv/fhr" ),
	  Data =lists:map( fun (X) -> html2data("priv/fhr/" ++ X) end, F ),
	  mnesia:transaction(
	    fun() -> lists:foreach(fun(X) -> addToTable(X) end,Data) end ).

start_db() ->
	mnesia:create_schema( [node()] ),
	{ok,_} = application:ensure_all_started( mnesia ).

addToTable( Data ) ->
	rund_tree:new( { init, maps:get( id, Data ) },
			{ init, getFather( Data ) },
			{ init,  getMother( Data ) },
			getBirthday( Data ),
		       	maps:remove( father, 
  		               maps:remove( mother, 
  			       maps:remove( id, 
			       maps:remove( geboortedatum, Data )))) ).
	

getFather( #{ father := #{ id := Id } } ) -> Id;
getFather( _ ) -> unknown.

getMother( #{ mother := #{ id := Id } } ) -> Id;
getMother( _ ) -> unknown.

getBirthday( #{ geboortedatum := BD } ) -> 
	case lists:map( fun (X) -> { I, <<"">> } = string:to_integer(X), I end, 
			binary:split( BD, <<"-">>, [global] ) ) of
		[ D, M, Y ] -> { Y, M, D }
	end;
getBirthday( _ ) -> unknown.

%% internal functions
%%
%% html2data( filename ) -> internal representation

%% tokenize
html2data( F ) ->
	{ ok, HTML } = file:read_file(F),
	scan_hdr( tokenizer:tokenize(HTML) ).

%% skip header (looking for the first data-cell (<td>)
scan_hdr( [ {<<"title">>,opening_tag, _ } | Rest ] ) -> 
	scan_hdr( log_title(Rest) );
scan_hdr( [ {<<"td">>, opening_tag, Options = [_|_] } | Rest ] ) -> 
	hdr_animal( Options, Rest, #{} );
scan_hdr( [ _ | Rest ] ) -> scan_hdr(Rest);
scan_hdr( [] ) -> eof.

%% First data is from tag-data
hdr_animal( [], File, Data = #{ sex := m } ) -> data_animal( File, Data );
hdr_animal( [], File, Data ) -> data_animal( File, Data#{ sex => f } );
hdr_animal( [{<<"rowspan">>,[ Gen ]}|R], File, Data ) -> 
	hdr_animal( R, File, Data#{ gen => Gen } );
%% males are shown in grey
hdr_animal( [{<<"bgcolor=\"lightgrey\"">>,[]}|R], File, Data ) 
	-> hdr_animal( R, File, Data#{ sex => m } ).

getName( [ { <<"a">>, closing_tag }|R], X ) -> data_animal(R,X);
getName( [ { text, Name } | R ], X ) -> getName( R, X#{ name => Name } );
getName( [ { <<"niet">>, _, _ } | R ], X ) -> getName( R, X ).

%% collect the data
data_animal( [ { <<"a">>,opening_tag, [{<<"href='", O/binary>>,[]}] }|R ], X )
	-> { Id, <<".html'">> } = string:to_integer(O),
	   getName( R, X#{ id => Id} );
data_animal( [ {<<"br">>,singleton_tag,[]}|R ], X ) -> data_animal( R,X );
data_animal( [ { text, <<"foknummer: ", T/binary >> }|R], X )
	-> data_animal(R, X#{foknummer => T} );
data_animal( [ { text, <<"geboortedatum: ", T/binary >> }|R], X )
	-> data_animal(R, X#{geboortedatum => T} );
data_animal( [ {<<"table">>,opening_tag,[]} | R ], X )
	-> { R2 , ML } = mlklst( R, [] ),
	   data_animal( R2, X#{ melklijst => ML } );
data_animal( [ {<<"niet">>, _, _}|R], X ) -> data_animal(R,X);
data_animal( [ {<<"td">>,closing_tag}|R ], X = #{ gen := <<"4">> } ) 
	-> find_parents( R, X ); % main animal
data_animal( [ {<<"td">>,closing_tag}|R ], X = #{ gen := <<"2">> } ) 
	->  { R, maps:remove( gen, X ) }; % parents
data_animal( [ {<<"td">>,closing_tag}|R ], _ ) 
	->  scan_hdr(R);
data_animal( [{<<"table">>,closing_tag}|_], _ ) -> eof.

find_parents( File, Data )
	-> case scan_hdr(File) of
		   { R, F = #{ sex := m } } -> 
			   find_parents( R, Data#{father=>F} );
		   { R, M = #{ sex := f } } -> 
			   find_parents( R, Data#{mother=>M} );
		   eof	->	maps:remove( gen, Data )
	   end.


skip( Tag, [{ Tag, closing_tag}|R] ) 
	-> R;
skip( Tag, [{ Tag, opening_tag, _ }| R ]) -> skip(Tag, skip( Tag, R ));
skip( Tag, [_|R] ) 
	   -> skip( Tag, R ).

mlklst( [{<<"tr">>,opening_tag,[]}|R], L ) 
	-> mlklst2( skip(<<"tr">> ,R), L ).

mlklst2( [{<<"tr">>,opening_tag,[]}|R], L )
	-> { R2, Row } = mlklst3( R, [] ),
	   mlklst2( R2, L ++ [Row] );
mlklst2( [{<<"table">>, closing_tag}|R] , L ) -> { R, L }.

mlklst3( [{<<"td">>,opening_tag,[]}|R], L ) -> mlklst3( R, L );
mlklst3( [{<<"td">>,closing_tag}|R], L ) -> mlklst3( R, L );
mlklst3( [{<<"tr">>,closing_tag}|R], L ) -> {R, L };
mlklst3( [{text, D}|R], L ) -> mlklst3( R, L ++ [D] ).

log_title( [ { text, T }| Rest ] ) -> io:format( "Processing: ~p~n", [T] ),
				      log_title(Rest);
log_title( [ { <<"title">>, closing_tag } | Rest ] ) -> Rest.
