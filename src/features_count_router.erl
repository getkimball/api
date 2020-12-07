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
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    add/1,
    add/2,
    add/4,
    add_goal/2,
    counts/0,
    count_map/0,
    counter_pids/0,
    goals/1,
    register_counter/2
]).

-record(state, {
    counters = [],
    goals = [],
    store_lib = undefined,
    store_lib_state = undefined
}).

-type counter_name() :: binary().
-type date_cohort() :: undefined | weekly.

-record(counter_config, {
    name :: counter_name(),
    date_cohort :: date_cohort()
}).

-record(counter_registration, {
    id :: any() | '_',
    pid,
    is_goal = false :: boolean() | '_'
}).

-define(COUNTER_CONFIG, feature_counter_config_table).
-define(COUNTER_REGISTRY, feature_counter_registry_table).
-define(DEFAULT_NAMESPACE, <<"default">>).
-define(PROM_COUNTER_NAME, kimball_counters).
-define(PROM_ADD_DURATION, kimball_event_add_duration_microseconds).
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

add(Items) when is_list(Items) ->
    [add(NS, C, K, O) || {NS, C, K, O} <- Items],
    ok.

add(CounterName, Key) ->
    add(?DEFAULT_NAMESPACE, CounterName, Key, #{}).

add(Namespace, CounterName, Key, Opts = #{ensure_goal := false}) ->
    Opts2 = maps:remove(ensure_goal, Opts),
    add(Namespace, CounterName, Key, Opts2);
add(Namespace, CounterName, Key, Opts = #{ensure_goal := true}) ->
    ensure_goal(Namespace, CounterName),
    Opts2 = maps:remove(ensure_goal, Opts),
    add(Namespace, CounterName, Key, Opts2);
add(Namespace, CounterName, Key, Opts) ->
    Start = erlang:monotonic_time(microsecond),
    Value = maps:get(value, Opts, undefined),
    YearWeekNum = calendar:iso_week_number(),

    CounterConfig = counter_config_for_name(CounterName),
    Counters = counters_for_event(Namespace, CounterName, CounterConfig, YearWeekNum),
    StartAndAdd = fun(CounterRegistration) ->
        ensure_started_and_add(Namespace, CounterRegistration, Key, Value)
    end,
    ok = lists:foreach(StartAndAdd, Counters),

    End = erlang:monotonic_time(microsecond),

    Duration = End - Start,
    prometheus_summary:observe(?PROM_ADD_DURATION, Duration),
    ok.

ensure_goal(Namespace, Goal) ->
    CR = ets:lookup(?COUNTER_REGISTRY, Goal),
    case CR of
        [#counter_registration{is_goal = true}] -> ok;
        _ -> add_goal(Namespace, Goal)
    end.

add_goal(Namespace, Goal) ->
    gen_server:call(?MODULE, {add_goal, Namespace, Goal}).

is_goal(CounterID) ->
    gen_server:call(?MODULE, {is_goal, CounterID}).

goals(Namespace) ->
    gen_server:call(?MODULE, {goals, Namespace}).

register_counter(CounterID, Pid) ->
    gen_server:cast(?MODULE, {register_counter, CounterID, Pid}).

counts() ->
    CountFun = fun(#counter_registration{id = CounterID, pid = Pid}, Acc0) ->
        Counts = features_counter:count(Pid),
        M = Counts#{id => CounterID},
        [M | Acc0]
    end,
    ets:foldl(CountFun, [], ?COUNTER_REGISTRY).

count_map() ->
    CountFun = fun(#counter_registration{id = CounterID, pid = Pid}, Acc0) ->
        Counts = features_counter:count(Pid),
        Acc0#{CounterID => Counts}
    end,
    ets:foldl(CountFun, #{}, ?COUNTER_REGISTRY).

counter_pids() ->
    PidFun = fun(#counter_registration{pid = Pid}, Acc0) ->
        [Pid | Acc0]
    end,
    ets:foldl(PidFun, [], ?COUNTER_REGISTRY).

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
    ?LOG_INFO(#{what => <<"features_count_router starting">>}),
    ?COUNTER_REGISTRY = ets:new(
        ?COUNTER_REGISTRY,
        [
            set,
            named_table,
            public,
            {read_concurrency, true},
            {keypos, #counter_registration.id}
        ]
    ),
    ?COUNTER_CONFIG = ets:new(
        ?COUNTER_CONFIG,
        [
            set,
            named_table,
            public,
            {read_concurrency, true},
            {keypos, #counter_config.name}
        ]
    ),
    persistent_term:put(?STORE_LIB_MOD_PT_KEY, StoreLib),
    StoreLibState = features_store_lib:init(
        StoreLib,
        <<"count_router">>
    ),

    prometheus_summary:declare([
        {name, ?PROM_ADD_DURATION},
        {help, "Time in microseconds to add an event"}
    ]),
    prometheus_gauge:declare([
        {name, ?PROM_COUNTER_NAME},
        {help, "Number of counters registered"}
    ]),
    gen_server:cast(self(), load_or_init),
    self() ! start_global_counter,
    {ok, #state{
        store_lib = StoreLib,
        store_lib_state = StoreLibState
    }}.

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
handle_call({add_goal, _Namespace, Goal}, _From, State = #state{goals = Goals}) ->
    % Ensure registration, if it exists, knows that this is a goal
    % TODO: Namespace this
    IDMatcher = features_counter_id:pattern_matcher_name(?DEFAULT_NAMESPACE, Goal),
    Matcher = #counter_registration{id = IDMatcher, pid = '_', is_goal = '_'},
    FeatureRegistrations = ets:match_object(?COUNTER_REGISTRY, Matcher),
    SetGoalRegistration = fun(Registration) ->
        NewRegistration = Registration#counter_registration{is_goal = true},
        true = ets:insert(?COUNTER_REGISTRY, NewRegistration)
    end,

    lists:foreach(SetGoalRegistration, FeatureRegistrations),

    % Include the goal in our internal list, then persist
    State1 =
        case lists:member(Goal, Goals) of
            true ->
                State;
            false ->
                Goals1 = [Goal | Goals],
                persist_state(State#state{goals = Goals1})
        end,
    Reply = ok,
    {reply, Reply, State1};
handle_call({is_goal, CounterID}, _From, State = #state{goals = Goals}) ->
    Name = features_counter_id:name(CounterID),
    Reply = lists:member(Name, Goals),
    {reply, Reply, State};
handle_call({goals, _Namespace}, _From, State = #state{goals = Goals}) ->
    % TODO: Namespace this
    Reply = Goals,
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
handle_cast(load_or_init, State = #state{store_lib_state = StoreLibState}) ->
    {Data, StoreLibState1} =
        case features_store_lib:get(StoreLibState) of
            {not_supported, NewState} -> {#{}, NewState};
            Else -> Else
        end,
    Counters = maps:get(counters, Data, []),
    Goals = maps:get(goals, Data, []),
    _Pids = [ensure_child_started(Counter) || Counter <- Counters],
    {noreply, State#state{
        counters = Counters,
        goals = Goals,
        store_lib_state = StoreLibState1
    }};
handle_cast(
    {register_counter, CounterID, Pid},
    State = #state{
        counters = Counters,
        goals = Goals
    }
) ->
    Name = features_counter_id:name(CounterID),
    IsGoal = lists:member(Name, Goals),
    CR = #counter_registration{
        id = CounterID,
        pid = Pid,
        is_goal = IsGoal
    },
    ets:insert(?COUNTER_REGISTRY, CR),
    State1 =
        case lists:member(CounterID, Counters) of
            true ->
                State;
            false ->
                NewCounters = [CounterID | Counters],
                persist_state(State#state{counters = NewCounters})
        end,
    update_prom_ets_counter(?COUNTER_REGISTRY, ?PROM_COUNTER_NAME),

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
    ensure_child_started(features_counter_id:global_counter_id(?DEFAULT_NAMESPACE)),
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
counters_for_event(
    Namespace,
    CounterName,
    #counter_config{date_cohort = DateCohort},
    {Year, WeekNum}
) ->
    GlobalRegistration = get_registration(
        features_counter_id:global_counter_id(Namespace)
    ),
    FeatureRegistration = get_registration(
        features_counter_id:create(Namespace, CounterName, named)
    ),

    WeekID = features_counter_id:create(Namespace, CounterName, weekly, {Year, WeekNum}),
    DateCohortRegistrations =
        case DateCohort of
            undefined -> [];
            weekly -> [get_registration(WeekID)]
        end,
    [GlobalRegistration, FeatureRegistration] ++ DateCohortRegistrations.

counter_config_for_name(Name) ->
    case ets:lookup(?COUNTER_CONFIG, Name) of
        [] ->
            LookedUpConfig = features_counter_config:config_for_counter(
                Name,
                init
            ),
            InitConfig =
                case LookedUpConfig of
                    undefined -> #{};
                    Else -> Else
                end,
            DateCohort = maps:get(date_cohort, InitConfig, undefined),
            CC = #counter_config{name = Name, date_cohort = DateCohort},
            true = ets:insert(?COUNTER_CONFIG, CC),
            CC;
        [LookedUpConfig] ->
            LookedUpConfig
    end.

ensure_child_started(CounterID) ->
    StoreLibMod = persistent_term:get(?STORE_LIB_MOD_PT_KEY),
    ?LOG_DEBUG(#{
        what => "Ensuring child started",
        counter_id => CounterID
    }),
    Spec = #{
        id => {features_counter, CounterID},
        start => {features_counter, start_link, [StoreLibMod, CounterID]}
    },
    StartInfo = supervisor:start_child(features_counter_sup, Spec),
    ?LOG_DEBUG(#{
        what => "Ensure Starting info",
        counter_id => CounterID,
        info => StartInfo
    }),
    Pid = pid_from_child_start(StartInfo),
    ?LOG_DEBUG(#{
        what => "got pid",
        pid => Pid
    }),
    Pid.

ensure_started_and_add(
    Namespace,
    CR = #counter_registration{id = CounterID, pid = undefined},
    Key,
    Value
) ->
    Pid = ensure_child_started(CounterID),
    IsGoal = is_goal(CounterID),
    R = CR#counter_registration{pid = Pid, is_goal = IsGoal},
    ensure_started_and_add(Namespace, R, Key, Value);
ensure_started_and_add(
    _Namespace,
    #counter_registration{pid = Pid, is_goal = false},
    Key,
    Value
) ->
    ok = features_counter:add(Key, Value, Pid);
ensure_started_and_add(
    Namespace,
    #counter_registration{pid = Pid, is_goal = true},
    Key,
    Value
) ->
    OtherCounters = named_counters_for_key(Namespace, Key),
    ok = features_counter:add(Key, OtherCounters, Value, Pid).

get_registration(CounterID) ->
    case ets:lookup(?COUNTER_REGISTRY, CounterID) of
        [] -> #counter_registration{id = CounterID};
        [R] -> R
    end.

named_counters_for_key(Namespace, Key) ->
    F = fun(#counter_registration{id = CounterID, pid = Pid}, AccIn) ->
        case features_counter:includes_key(Key, Pid) of
            true -> [counter_id_to_tag(CounterID) | AccIn];
            false -> AccIn
        end
    end,
    IDMatcher = features_counter_id:pattern_matcher_type(Namespace, named),
    Matcher = #counter_registration{id = IDMatcher, pid = '_', is_goal = '_'},
    InternalCounters = ets:match_object(?COUNTER_REGISTRY, Matcher),

    CountersForKey = lists:foldl(F, [], InternalCounters),
    CountersForKey.

counter_id_to_tag(ID) ->
    Name = features_counter_id:name(ID),
    Name.

pid_from_child_start({_, Pid}) when is_pid(Pid) ->
    Pid;
pid_from_child_start({_, {_, Pid}}) when is_pid(Pid) ->
    Pid;
pid_from_child_start(Else) ->
    throw({aaaaah, Else}).

persist_state(
    State = #state{
        counters = Counters,
        goals = Goals,
        store_lib_state = StoreLibState
    }
) ->
    PersistData = #{
        counters => Counters,
        goals => Goals
    },
    % At the moment the only options are ok and not_supported, not much we
    % can/should do here if things are supported
    {_Status, StoreLibState1} = features_store_lib:store(
        PersistData,
        StoreLibState
    ),
    State#state{store_lib_state = StoreLibState1}.

update_prom_ets_counter(Tab, GaugeName) ->
    TabInfo = ets:info(Tab),
    Size = proplists:get_value(size, TabInfo),
    prometheus_gauge:set(GaugeName, Size).
