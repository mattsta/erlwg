-module(erlwg_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0, start_getter/2, start_getter/3]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_getter(Name, Interval) when is_atom(Name) andalso is_integer(Interval) ->
  supervisor:start_child(?MODULE, [Name, Interval]).

start_getter(Name, Interval, TransformFun)
    when is_atom(Name) andalso
         is_integer(Interval) andalso
         is_function(TransformFun) ->
  supervisor:start_child(?MODULE, [Name, Interval, TransformFun]).

init([]) ->

  WG = {erlwg_server,
        {erlwg_server, start_link, []},
        permanent, 5000, worker, [erlwg_server]},
  Processes = [WG],

  Strategy = {simple_one_for_one, 10, 10},
  {ok,
   {Strategy, lists:flatten(Processes)}}.
