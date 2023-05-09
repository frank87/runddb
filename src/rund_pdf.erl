-module(rund_pdf).

-include_lib( "erlguten/include/eg.hrl" ).
-export( [ tree/1, test/0, test/1 ] ).


tree( Id ) -> Data = rund_tree:get_tree( Id, 4 ),
	      PDF = eg_pdf:new(),
	      eg_pdf:set_pagesize(PDF,a4),

	      eg_pdf:set_page(PDF,1),
	      print_tree( PDF, Data, eg_pdf:pagesize(a4), 1, maxLevel(Data,1) ),
	      Rv = eg_pdf:export(PDF),
	      eg_pdf:delete(PDF),
	      Rv.



print_tree( PDF, Data, { X0, Y0, X1, Y1 }, MyLevel, MaxLevel ) ->
       	next_tree( PDF, Data, vader, { X0, ( Y0 + Y1 ) div 2, X1, Y1 }, MyLevel, MaxLevel ),
       	next_tree( PDF, Data, moeder, { X0, Y0, X1, ( Y0 + Y1 ) div 2 }, MyLevel, MaxLevel ),
	print_leaf( PDF, Data, { X0, Y0, X1, Y1 }, MyLevel, MaxLevel ).
print_leaf( PDF, Data, { X0, Y0, X1, Y1 }, MyLevel, MaxLevel ) when MyLevel > 0 ->
	      eg_table:table( PDF, 
			      table(Data),
			      X0 + ( ( MyLevel - 1 )  * ( X1 - X0 ) ) div MaxLevel,
			      ( X1 - X0 ) div MaxLevel,
			      ( Y1 + Y0 ) div 2, 
			      Y0, 
			      10, 
			      #table{} 
			    );
print_leaf( _, _, _, _, _ ) -> ok.

next_tree( PDF, Data, Key, Box, MyLevel, MaxLevel ) when is_map( map_get(Key,Data) ) ->
       print_tree( PDF, map_get(Key, Data), Box, MyLevel + 1, MaxLevel );
next_tree( _PDF, _Data, _Key, _Box, _MyLevel, _MaxLevel ) -> ok.

maxLevel( Data, MyLevel ) ->
	lists:max(
	  [
	   maxLevel( Data, vader, MyLevel ),
	   maxLevel( Data, moeder, MyLevel )
	  ] ).


maxLevel( Data, Key, MyLevel ) when is_map( map_get(Key,Data) ) ->
       maxLevel( map_get(Key, Data), MyLevel + 1 );
maxLevel( _Data, _Key, MyLevel ) -> MyLevel.

table( #{ name := Name, geboortedatum := Birthdate } ) ->
	[
	 row( "naam", Name ),
	 row( "geb. datum", Birthdate )
       	].

row( Title, Data ) when is_binary( Data ) -> row( Title, binary_to_list( Data ) );
row( Title, { Y, M, D } ) -> row( Title, io_lib:fwrite("~B-~B-~B", [ D, M, Y ] ) );
row( Title, Data ) ->
	{ row, [], [
		    {cell, [], [ { raw, Title } ] },
		    {cell, [], [ { raw, Data } ] }
		   ]}.

test( X ) -> 
	{ PDF, _ } = tree({init,X}),
	file:write_file( "test.pdf", PDF ).

test() ->
	test(2).
