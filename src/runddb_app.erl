%%%-------------------------------------------------------------------
%% @doc runddb public API
%% @end
%%%-------------------------------------------------------------------

-module(runddb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> 
	start_web(),
	runddb_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
%%
start_web() ->
	Dispatch = cowboy_router:compile(
		     [
		      {'_', 
		       [
			%{"/kwartierstaat/:id[/:dept]", rund_kwartier, [] }
			{"/js/[...]", cowboy_static, {priv_dir, runddb, "js"}},
			{"/html/[...]", cowboy_static, {priv_dir, runddb, "html"}},
			{"/kwartierstaat/:id", rund_kwartier, [] },
			{"/rund", rund_kwartier, [] },
			{"/", rund_stierlijst, [] }
		       ] 
		      }
		     ]
		    ),
	{ ok, _ } = cowboy:start_clear( rund,
					[{port, 8080}],
					#{env => #{dispatch => Dispatch } }
				      ).
					  
