-module(erlwg_server).
-behaviour(gen_server).

-compile(export_all).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/2, start_link/3, get/3]).

-type epoch() :: pos_integer().

-record(state, {interval         :: pos_integer(),
                tfun             :: function(),
                cache            :: [{atom(), {epoch(), binary()}}]
               }).

%%%--------------------------------------------------------------------
%%% api callbacks
%%%--------------------------------------------------------------------
start_link(GenServerName, Interval) when is_integer(Interval) ->
  start_link(GenServerName, Interval, fun(E) -> E end).

start_link(GenServerName, Interval, TransformFun) ->
  gen_server:start_link({local, GenServerName}, ?MODULE,
                        [Interval, TransformFun], []).

get(Server, ResourceName, URL) ->
  gen_server:call(Server, {get, ResourceName, URL}).

%%%--------------------------------------------------------------------
%%% gen_server callbacks
%%%--------------------------------------------------------------------

init([Interval, TransformFun]) ->
  {ok, #state{interval = Interval,
              tfun = TransformFun,
              cache = []}}.

handle_call({get, ResourceName, URL}, _From,
    #state{interval=I, cache=C, tfun=F} = State) ->
  Now = n2s(),
  WhatToDo = case proplists:get_value(ResourceName, C) of
               undefined -> pg;
               {When, Contents} when (Now - When) > I -> {pg, Contents};
               {_, Contents} -> Contents
             end,
  case WhatToDo of
            pg -> Got = process_get(self(), ResourceName, URL, F),
                  NewState = update_cache_state(ResourceName, Got, State),
                  {reply, Got, NewState};
    {pg, Data} -> Self = self(),
                  spawn(fun() -> process_get(Self, ResourceName, URL, F) end),
                  % NewState is here so we bump the last updated time.
                  % Make sure we only spawn one get every > Interval seconds.
                  NewState = update_cache_state(ResourceName, Data, State),
                 {reply, Data, NewState};
          Data -> {reply, Data, State}
  end.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({got, Name, Got}, State) ->
  {noreply, update_cache_state(Name, Got, State)};

handle_info(Info, State) ->
  lager:error("Other info: ~p with state ~p~n", [Info, State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%--------------------------------------------------------------------
%%% URL Getting
%%%--------------------------------------------------------------------

-spec process_get(any(), atom(), list(), function()) ->
    death | {got, atom(), binary()}.
process_get(UpdatePid, ResourceName, URL, TransformFun) ->
  case get_URL(URL) of
    error -> TransformFun(death);
      Got -> Transformed = TransformFun(Got),
             UpdatePid ! {got, ResourceName, Transformed},
             Transformed
  end.

n2s() -> now_to_seconds(now()).

now_to_seconds({Mega, Sec, _}) ->
  (Mega * 1000000) + Sec.

update_cache_state(Name, Contents, #state{cache = C} = State) ->
  State#state{cache = lists:keystore(Name, 1, C, {Name, {n2s(), Contents}})}.

get_URL(URL) ->
  case httpc:request(get, {URL, []}, [], [{body_format, binary}]) of
    {ok, {_, _, Body}} -> Body;
                     _ -> error
  end.
