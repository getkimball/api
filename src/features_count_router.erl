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

-record(state, {}).

-record(counter_registration, {name,
                               pid}).

-define(COUNTER_REGISTRY, feature_counter_registry_table).
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
    CounterRegistration = ets:lookup(?COUNTER_REGISTRY, CounterName),
    Tab = ets:tab2list(?COUNTER_REGISTRY),
    ?LOG_DEBUG(#{what=>"Router add 1",
                 name=>CounterName,
                 registration=>CounterRegistration,
                 tab => Tab,
                 key=>Key}),
    Pid = case CounterRegistration of
        [] -> ?LOG_DEBUG(#{what=>"Router add 1.1",
                 name=>CounterName,
                 registration=>CounterRegistration,
                 key=>Key}),
              P = ensure_child_started(CounterName),
              ?LOG_DEBUG(#{what=>"Router add 1.2",
                 name=>CounterName,
                 registration=>CounterRegistration,
                 key=>Key}),
              P;
        [CounterRegistrationItem] ->
            CounterRegistrationItem#counter_registration.pid
    end,
    ?LOG_DEBUG(#{what=>"Router add 2",
                 name=>CounterName,
                 registration=>CounterRegistration,
                 pid=>Pid,
                 key=>Key}),
    ok = features_counter:add(Key, Pid),
    ?LOG_DEBUG(#{what=>"Router add 3",
                 name=>CounterName,
                 registration=>CounterRegistration,
                 pid=>Pid,
                 key=>Key}),

    ok.

register_counter(CounterName, Pid) ->
    CR = #counter_registration{name=CounterName, pid=Pid},
    ets:insert(?COUNTER_REGISTRY, CR),
    ?LOG_DEBUG(#{what=>"Counter registration",
                 name=>CounterName,
                 pid=>Pid }),
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
    ?COUNTER_REGISTRY = ets:new(?COUNTER_REGISTRY,
                                [set,
                                 named_table,
                                 public,
                                 {read_concurrency, true},
                                 {keypos, #counter_registration.name}]),
    {ok, #state{}}.

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
ensure_child_started(FeatureName) ->
    ?LOG_DEBUG(#{what=>"Starting child",
                 feature => FeatureName}),
    Spec = #{id => {features_counter, FeatureName},
             start => {features_counter, start_link, [FeatureName]}},
    StartInfo =  supervisor:start_child(features_counter_sup, Spec),
    ?LOG_DEBUG(#{what=>"Starting info",
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
