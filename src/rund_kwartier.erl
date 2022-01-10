-module(rund_kwartier).

-export( [ init/2 ] ).
-export( [ allowed_methods/2 ] ).
-export( [ content_types_provided/2 ] ).

-export( [ to_html/2 ] ).
-export( [ to_json/2 ] ).

init(Req0, State) -> 
	{cowboy_rest, Req0, State }.

allowed_methods(Req, State ) ->
	{ [<<"GET">>], Req, State }.

content_types_provided(Req, State) ->
	io:format("request: ~p~n", [ cowboy_req:headers(Req) ] ),
	{[
	  {{<<"text">>,<<"html">>, '*'}, to_html },
	  {{<<"application">>,<<"json">>, '*' }, to_json }
	], Req, State}.

get_data( Req, _State, Dept ) ->
	Id = cowboy_req:binding( id, Req ),
	io:format("Req -> ~p~n", [ Req ] ),
	case string:to_integer( Id ) of
		{ error, _ } -> rund_tree:get_tree( Id, Dept );
		{ IdNr, _ } ->  rund_tree:get_tree( { init, IdNr }, Dept )
	end.

to_html( Req, State ) ->
	io:format("~p~n", [ erlydtl:compile_dir("priv/erlydtl", dtl ) ] ),
	Tree = get_data( Req, State, 3 ),
	Out = dtl:kwartier( #{ animal => Tree } ),
	{ Out, Req, State }.


filtermap( F, M ) ->
	List = maps:to_list( M ),
	Res = lists:filtermap( fun( { K, V } ) -> case F( K, V ) of
							  { true, V1 } -> { true, { K,V1 } };
							  X	-> X
						  end
						  end, List ),
	maps:from_list( Res ).

date2ISO( { Y, M, D } ) -> list_to_binary( io_lib:format("~p-~p-~p", [ Y, M, D ] ) );
date2ISO( M ) when is_map(M) ->
       	filtermap( fun( _K, V ) -> { true, date2ISO( V ) } end, M );
date2ISO( { Type, Data } ) -> io_lib:format( "~w_~w", [ Type, Data ]);
date2ISO( D ) -> 
	D.

to_json( Req, State ) ->
	Data = date2ISO( get_data( Req, State, 1 ) ),
	Out = jsone:encode( Data ),
	io:format("~p -> ~p~n", [ Data, Out ] ),
	{ Out, Req, State }.
