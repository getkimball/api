%%%-------------------------------------------------------------------
%%% @author $AUTHOR
%%% @copyright 2020 $OWNER
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(features_count_router).
-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).

%% API functions
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-export([add/2,
         add/3,
         add_goal/1,
         counts/0,
         goals/0,
         register_counter/2]).

-record(state, {counters=[],
                goals=[],
                store_lib=undefined,
                store_lib_state=undefined}).

-record(counter_registration, {name,
                               pid,
                               is_goal=false}).

-define(COUNTER_REGISTRY, feature_counter_registry_table).
-define(GLOBAL_COUNTER, global_counter).
-define(STORE_LIB_MOD, features_store_lib_s3).
-define(STORE_LIB_MOD_PT_KEY, features_count_router_store_lib_mod).
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
start_link(StoreLib) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [StoreLib], []).

add(CounterName, Key) ->
    add(CounterName, Key, #{}).

add(CounterName, Key, Opts=#{ensure_goal:=false}) ->
    Opts2 = maps:remove(ensure_goal, Opts),
    add(CounterName, Key, Opts2);
add(CounterName, Key, Opts=#{ensure_goal:=true}) ->
    ensure_goal(CounterName),
    Opts2 = maps:remove(ensure_goal, Opts),
    add(CounterName, Key, Opts2);
add(CounterName, Key, _Opts) ->
    GlobalRegistration = ets:lookup(?COUNTER_REGISTRY, ?GLOBAL_COUNTER),
    ensure_started_and_add(?GLOBAL_COUNTER, GlobalRegistration, Key),
    FeatureRegistration = ets:lookup(?COUNTER_REGISTRY, CounterName),
    ensure_started_and_add(CounterName, FeatureRegistration, Key),
    ?LOG_DEBUG(#{what=>"Router add 2",
                 name=>CounterName,
                 feature_registration=>FeatureRegistration,
                 glocal_registration=>GlobalRegistration,
                 key=>Key}),

    ok.

ensure_goal(Goal) ->
    CR = ets:lookup(?COUNTER_REGISTRY, Goal),
    case CR of
        [#counter_registration{name=Goal, % just to ensure it's right
                               is_goal=true}] -> ok;
        _ -> add_goal(Goal)
    end.

add_goal(Goal) ->
    gen_server:call(?MODULE, {add_goal, Goal}).

is_goal(Goal) ->
    gen_server:call(?MODULE, {is_goal, Goal}).

goals() ->
    gen_server:call(?MODULE, goals).

register_counter(CounterName, Pid) ->
    gen_server:cast(?MODULE, {register_counter, CounterName, Pid}).

counts() ->
    CountFun = fun(#counter_registration{name=CounterName, pid=Pid}, Acc0) ->
        #{count := Count,
          tag_counts := TagCounts} = features_counter:count(Pid),
        M = #{name => CounterName,
              count => Count,
              tag_counts => TagCounts},
        [M | Acc0]
    end,
    ets:foldl(CountFun, [], ?COUNTER_REGISTRY).


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
init([StoreLib]) ->
    ?LOG_INFO(#{what=><<"features_count_router starting">>}),
    ?COUNTER_REGISTRY = ets:new(?COUNTER_REGISTRY,
                                [set,
                                 named_table,
                                 public,
                                 {read_concurrency, true},
                                 {keypos, #counter_registration.name}]),
    persistent_term:put(?STORE_LIB_MOD_PT_KEY, StoreLib),
    StoreLibState = features_store_lib:init(StoreLib,
                                            "count_router"),
    gen_server:cast(self(), load_or_init),
    self() ! start_global_counter,
    {ok, #state{store_lib=StoreLib,
                store_lib_state=StoreLibState}}.

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
handle_call({add_goal, Goal}, _From, State=#state{goals=Goals}) ->
    % Ensure registration, if it exists, knows that this is a goal
    %
    FeatureRegistration = ets:lookup(?COUNTER_REGISTRY, Goal),
    case FeatureRegistration of
        [] -> ok;
        [CR] -> GoalRegistration = CR#counter_registration{is_goal=true},
                true = ets:insert(?COUNTER_REGISTRY, GoalRegistration)
    end,
    % Include the goal in our internal list, then persist
    State1 = case lists:member(Goal, Goals) of
        true -> State;
        false -> Goals1 = [Goal|Goals],
                 persist_state(State#state{goals=Goals1})
    end,
    Reply = ok,
    {reply, Reply, State1};
handle_call({is_goal, Goal}, _From, State=#state{goals=Goals}) ->
    Reply = lists:member(Goal, Goals),
    {reply, Reply, State};
handle_call(goals, _From, State=#state{goals=Goals}) ->
    Reply = Goals,
    {reply, Reply, State};
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
handle_cast(load_or_init, State=#state{store_lib_state=StoreLibState}) ->
    {Data, StoreLibState1} = case features_store_lib:get(StoreLibState) of
        {not_supported, NewState} -> {#{}, NewState};
        Else -> Else
    end,
    Counters = maps:get(counters, Data, []),
    Goals = maps:get(goals, Data, []),
    _Pids = [ensure_child_started(Counter) || Counter <- Counters],
    {noreply, State#state{counters=Counters,
                          goals=Goals,
                          store_lib_state=StoreLibState1}};
handle_cast({register_counter, CounterName, Pid},
            State=#state{counters=Counters,
                         goals=Goals}) ->

    IsGoal = lists:member(CounterName, Goals),
    CR = #counter_registration{name=CounterName,
                               pid=Pid,
                               is_goal=IsGoal},
    ets:insert(?COUNTER_REGISTRY, CR),
    State1 = case lists:member(CounterName, Counters) of
        true -> State;
        false -> NewCounters = [CounterName|Counters],
                 persist_state(State#state{counters=NewCounters})
    end,
    {noreply, State1};
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
handle_info(start_global_counter, State) ->
    ensure_child_started(?GLOBAL_COUNTER),
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
ensure_child_started(FeatureName) ->
    StoreLibMod = persistent_term:get(?STORE_LIB_MOD_PT_KEY),
    ?LOG_DEBUG(#{what=>"Ensuring child started",
                 feature => FeatureName}),
    Spec = #{id => {features_counter, FeatureName},
             start => {features_counter,
                       start_link,
                       [StoreLibMod, FeatureName]}},
    StartInfo =  supervisor:start_child(features_counter_sup, Spec),
    ?LOG_DEBUG(#{what=>"Ensure Starting info",
                 feature => FeatureName,
                 info => StartInfo}),
    Pid = pid_from_child_start(StartInfo),
    ?LOG_DEBUG(#{what=>"got pid",
                 pid => Pid}),
    Pid.

ensure_started_and_add(Name, [], Key) ->
    Pid = ensure_child_started(Name),
    IsGoal = is_goal(Name),
    R = #counter_registration{pid=Pid, is_goal=IsGoal},
    ensure_started_and_add(Name, [R], Key);
ensure_started_and_add(_Name,
                       [#counter_registration{pid=Pid, is_goal=false}],
                       Key) ->
    ok = features_counter:add(Key, Pid);
ensure_started_and_add(_Name,
                       [#counter_registration{pid=Pid, is_goal=true}],
                       Key) ->
    OtherCounters = counters_for_key(Key),
    ok = features_counter:add(Key, OtherCounters, Pid).

counters_for_key(Key) ->
    F = fun(#counter_registration{name=Name, pid=Pid}, AccIn) ->
            case features_counter:includes_key(Key, Pid) of
                true -> [Name| AccIn];
                false -> AccIn
            end
    end,
    Counters = ets:foldl(F, [], ?COUNTER_REGISTRY),
    Counters.

pid_from_child_start({_, Pid}) when is_pid(Pid) ->
    Pid;
pid_from_child_start({_, {_, Pid}}) when is_pid(Pid) ->
    Pid;
pid_from_child_start(Else) ->
    throw({aaaaah, Else}).

persist_state(State=#state{counters=Counters,
                           goals=Goals,
                           store_lib_state=StoreLibState}) ->
    PersistData = #{
        counters => Counters,
        goals => Goals
    },
    % At the moment the only options are ok and not_supported, not much we
    % can/should do here if things are supported
    {_Status, StoreLibState1} = features_store_lib:store(
                                        PersistData,
                                        StoreLibState),
    State#state{store_lib_state=StoreLibState1}.
