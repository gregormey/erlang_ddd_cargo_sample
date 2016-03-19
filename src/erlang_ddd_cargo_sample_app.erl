-module(erlang_ddd_cargo_sample_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	erlang_ddd_cargo_sample_sup:start_link().

stop(_State) ->
	ok.
