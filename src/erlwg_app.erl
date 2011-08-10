-module(erlwg_app).

-behaviour(application).
-export([start/2,stop/1]).

-spec start(any(), any()) -> any().
start(_Type, _StartArgs) ->
  ibrowse:start(),
  erlwg_sup:start_link().

-spec stop(any()) -> any().
stop(_State) ->
  ok.
