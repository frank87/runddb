%%%-------------------------------------------------------------------
%% @doc runddb public API
%% @end
%%%-------------------------------------------------------------------

-module(rund_util).

-export([init/0, uniq_id/1 ]).

-record( counter, { label, last } ).

init() -> 
	mnesia:create_table( counter,
			     [ 
			      {attributes, record_info( fields, counter ) },
			      {disc_copies, [ node() ] }
			     ] ).

uniq_id( Label ) -> mnesia:dirty_update_counter( counter, Label, 1 ).

%% internal functions
