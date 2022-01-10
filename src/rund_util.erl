%%%-------------------------------------------------------------------
%% @doc runddb public API
%% @end
%%%-------------------------------------------------------------------

-module(rund_util).

-export([init/0, uniq_id/1 ]).
-export([encode/1, decode/1 ]).

-record( counter, { label, last } ).

init() -> 
	mnesia:create_table( counter,
			     [ 
			      {attributes, record_info( fields, counter ) },
			      {disc_copies, [ node() ] }
			     ] ).

uniq_id( Label ) -> mnesia:dirty_update_counter( counter, Label, 1 ).


encode( D ) -> encode(term_to_binary(D), <<>> ).

decode( D ) -> 
	io:format("[~w]~n", [ D ] ),
	binary_to_term(decode(D,<<>>)).
%% internal functions
%%
encode( <<>>, A ) -> A;
encode( <<D:4, R/bitstring >>, A ) -> encode( R, << A/bitstring, (D + 65) >> ).

decode( <<>>, A ) -> A;
decode( <<D, Rest/bitstring>>, A  ) -> decode( Rest, << A/bitstring, (D-65):4 >> ).
