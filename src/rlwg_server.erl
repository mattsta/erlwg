-module(rlwg_server).
-behaviour(gen_server).

-compile(export_all).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/2, get/3]).

-type epoch() :: pos_integer().

-record(state, {interval         :: pos_integer(),
                cache            :: [{atom(), {epoch(), binary()}}]
               }).

%%%--------------------------------------------------------------------
%%% api callbacks
%%%--------------------------------------------------------------------
start_link(GenServerName, Interval) when is_integer(Interval) ->
  gen_server:start_link({local, GenServerName}, ?MODULE, [Interval], []).

get(Server, ResourceName, URL) ->
  gen_server:call(Server, {get, ResourceName, URL}).

%%%--------------------------------------------------------------------
%%% gen_server callbacks
%%%--------------------------------------------------------------------

init([Interval]) ->
  {ok, #state{interval = Interval,
              cache = []}}.

handle_call({get, ResourceName, URL},_From,#state{interval=I,cache=C}=State) ->
  Now = n2s(),
  WhatToDo = case proplists:get_value(ResourceName, C) of
               undefined -> pg;
               {When, Contents} when (Now - When) > I -> {pg, Contents};
               {_, Contents} -> Contents
             end,
  case WhatToDo of
    pg -> Got = process_get(self(), ResourceName, URL),
          NewState = update_cache_state(ResourceName, Got, State),
          {reply, Got, NewState};
    {pg, C} -> spawn(fun() -> process_get(self(), ResourceName, URL) end),
                % NewState is here so we bump the last updated time.
                % This makes sure we only spawn one get every > Interval seconds
                NewState = update_cache_state(ResourceName, C, State),
               {reply, C, NewState};
    C -> {reply, C, State}
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

-spec process_get(any(), atom(), list()) -> death | {got, atom(), binary()}.
process_get(UpdatePid, ResourceName, URL) ->
  case get_URL(URL) of
    error -> death;
      Got -> UpdatePid ! {got, ResourceName, Got}
  end.

n2s() -> now_to_seconds(now()).

now_to_seconds({Mega, Sec, _}) ->
  (Mega * 1000000) + Sec.

update_cache_state(Name, Contents, #state{cache = C} = State) ->
  State#state{cache = lists:keystore(Name, 1, C, {Name, n2s(), Contents})}.

get_URL(URL) ->
  case ibrowse:send_req(URL, [], get, [], [{response_format, binary}], 3000) of
    {ok, _, _, Body} -> Body;
                   _ -> error
  end.
