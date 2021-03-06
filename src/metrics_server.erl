%%%-------------------------------------------------------------------
%%% @author $AUTHOR
%%% @copyright 2020 $OWNER
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(metrics_server).

-behaviour(gen_server).

%% API functions
-export([
    start_link/1,
    tick/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {memory_limit}).

-define(PROM_MEM_REMAINING, memory_remaining_bytes).
-define(PROM_BAYES_PREDICTION, kimball_bayes_prediction).
-define(PROM_PREDICTIONS_REGISTRY, predictions).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

tick() ->
    gen_server:cast(?MODULE, tick).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Opts]) when is_map(Opts) ->
    MemoryLimit = maps:get(memory_limit, Opts, 0),

    prometheus_gauge:declare([
        {name, ?PROM_MEM_REMAINING},
        {help, "Bytes of memory remaining as viewed by the Erlang VM"}
    ]),

    prometheus_gauge:declare([
        {name, ?PROM_BAYES_PREDICTION},
        {help, "Prediction that event will lead to goal"},
        {labels, [goal, event]},
        {registry, ?PROM_PREDICTIONS_REGISTRY}
    ]),

    {ok, _TRef} = timer:apply_interval(15000, ?MODULE, tick, []),
    {ok, #state{memory_limit = MemoryLimit}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(tick, State) ->
    S1 = run_memory_metric(State),
    S2 = run_bayes_prediction_metric(S1),
    {noreply, S2};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

run_memory_metric(State = #state{memory_limit = MemLimit}) ->
    MemInfo = erlang:memory(),
    TotalMem = proplists:get_value(total, MemInfo),
    MemRemaining = MemLimit - TotalMem,
    prometheus_gauge:set(?PROM_MEM_REMAINING, MemRemaining),
    State.

run_bayes_prediction_metric(State) ->
    % TODO run for each namespace?
    PredictionsMap = features_bayesian_predictor:for_goal_counts(<<"default">>),
    PredictionsList = maps:to_list(PredictionsMap),

    lists:foreach(fun for_prediction_goal_events/1, PredictionsList),

    State.

for_prediction_goal_events({Goal, EventsMap}) ->
    ForEventFun = fun({Event, Val}) ->
        prometheus_gauge:set(?PROM_PREDICTIONS_REGISTRY, ?PROM_BAYES_PREDICTION, [Goal, Event], Val)
    end,
    EventsList = maps:to_list(EventsMap),
    lists:foreach(ForEventFun, EventsList).
