-module(rund_kwartier).

-export( [ init/2 ] ).

init(Req0, State) -> 
	{ Id, _ } = string:to_integer( 
		      cowboy_req:binding( id, Req0 ) 
		     ),
	%{ Dept, _ } = string:to_integer( 
			%cowboy_req:binding( dept, Req0, <<"2">> )
		       %),
	Dept = 3,
	io:format("~p~n", [ erlydtl:compile_dir("priv/erlydtl", dtl ) ] ),
	Tree = rund_tree:get_tree( { init, Id }, Dept ),
	{ ok, Out } = dtl:kwartier( #{ animal => Tree } ),
	io:format("~p~n", [Out] ),
	Req = cowboy_req:reply( 200, 
				#{<<"content-type">> => <<"text/html">>},
				Out,
			       	Req0),
	{ok, Req, State}.
