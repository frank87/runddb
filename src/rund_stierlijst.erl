-module(rund_stierlijst).

-export( [ init/2 ] ).

init(Req0, State) -> 
	erlydtl:compile("priv/erlydtl/stierlijst.html", stierlijst_dtl ),
	List = rund_tree:get_bulls(),
	{ ok, Out } = stierlijst_dtl:render( #{ bulls => List } ),
	io:format("~p~n", [Out] ),
	Req = cowboy_req:reply( 200, 
				#{<<"content-type">> => <<"text/html">>},
				Out,
			       	Req0),
	{ok, Req, State}.
