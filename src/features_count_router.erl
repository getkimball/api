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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-export([add/2,
         counts/0,
         register_counter/2]).

-record(state, {counters=[],
                store_lib_state=undefined}).

-record(counter_registration, {name,
                               pid}).

-define(COUNTER_REGISTRY, feature_counter_registry_table).
-define(GLOBAL_COUNTER, global_counter).
-define(STORE_LIB_MOD, features_store_lib_s3).
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
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(CounterName, Key) ->
    FeatureRegistration = ets:lookup(?COUNTER_REGISTRY, CounterName),
    ensure_started_and_add(CounterName, FeatureRegistration, Key),
    GlobalRegistration = ets:lookup(?COUNTER_REGISTRY, ?GLOBAL_COUNTER),
    ensure_started_and_add(?GLOBAL_COUNTER, GlobalRegistration, Key),
    ?LOG_DEBUG(#{what=>"Router add 2",
                 name=>CounterName,
                 feature_registration=>FeatureRegistration,
                 glocal_registration=>GlobalRegistration,
                 key=>Key}),

    ok.

ensure_started_and_add(Name, [], Key) ->
    Pid = ensure_child_started(Name),
    ok = features_counter:add(Key, Pid);
ensure_started_and_add(_Name, [Registration], Key) ->
    Pid = Registration#counter_registration.pid,
    ok = features_counter:add(Key, Pid).

register_counter(CounterName, Pid) ->
    CR = #counter_registration{name=CounterName, pid=Pid},
    ets:insert(?COUNTER_REGISTRY, CR),
    ?LOG_DEBUG(#{what=>"Counter registration",
                 name=>CounterName,
                 pid=>Pid }),
    gen_server:cast(?MODULE, {counter_registered, CounterName}),
    ok.

counts() ->
    CountFun = fun(#counter_registration{name=CounterName, pid=Pid}, Acc0) ->
        Count = features_counter:count(Pid),
        M = #{name => CounterName, count => Count},
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
init([]) ->
    ?LOG_INFO(#{what=><<"features_count_router starting">>}),
    ?COUNTER_REGISTRY = ets:new(?COUNTER_REGISTRY,
                                [set,
                                 named_table,
                                 public,
                                 {read_concurrency, true},
                                 {keypos, #counter_registration.name}]),
    StoreLibState = features_store_lib:init(?STORE_LIB_MOD,
                                            "count_router"),
    gen_server:cast(self(), load_or_init),
    self() ! start_global_counter,
    {ok, #state{store_lib_state=StoreLibState}}.

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
handle_cast(load_or_init, State=#state{store_lib_state=StoreLibState}) ->
    {Data, StoreLibState1} = features_store_lib:get(StoreLibState),
    Counters = maps:get(counters, Data, []),
    _Pids = [ensure_child_started(Counter) || Counter <- Counters],
    {noreply, State#state{store_lib_state=StoreLibState1}};
handle_cast({counter_registered, CounterName},
            State=#state{counters=Counters}) ->
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
    ?LOG_DEBUG(#{what=>"Ensuring child started",
                 feature => FeatureName}),
    Spec = #{id => {features_counter, FeatureName},
             start => {features_counter,
                       start_link,
                       [?STORE_LIB_MOD, FeatureName]}},
    StartInfo =  supervisor:start_child(features_counter_sup, Spec),
    ?LOG_DEBUG(#{what=>"Ensure Starting info",
                 feature => FeatureName,
                 info => StartInfo}),
    Pid = pid_from_child_start(StartInfo),
    ?LOG_DEBUG(#{what=>"got pid",
                 pid => Pid}),
    Pid.

pid_from_child_start({_, Pid}) when is_pid(Pid) ->
    Pid;
pid_from_child_start({_, {_, Pid}}) when is_pid(Pid) ->
    Pid;
pid_from_child_start(Else) ->
    throw({aaaaah, Else}).

persist_state(State=#state{counters=Counters,
                           store_lib_state=StoreLibState}) ->
    PersistData = #{
        counters => Counters
    },
    {ok, StoreLibState1} = features_store_lib:store(PersistData, StoreLibState),
    State#state{store_lib_state=StoreLibState1}.
