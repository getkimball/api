-module(features_store).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

%% API functions
-export([
    start_link/1,
    start_link/2
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

-export([
    set_feature/4,
    get_features/0,
    refresh_from_store/0
]).

-record(state, {
    refresh_interval = undefined,
    store_lib_state = undefined
}).

-record(rollout_spec, {
    start = undefined,
    'end' = undefined
}).

-record(user_spec, {
    prop = undefined,
    comparator_atom = undefined,
    value = undefined
}).

-record(feature, {
    name = undefined,
    boolean = false,
    rollout = #rollout_spec{},
    user_specs = []
}).

-define(FEATURE_REGISTRY, feature_registry_table).

%%%===================================================================
%%% API functions
%%%===================================================================

set_feature(
    Name,
    {boolean, Boolean},
    {rollout, Start, End},
    {user, UserSpecIn}
) ->
    R = #rollout_spec{start = Start, 'end' = End},
    US = build_user_specs(UserSpecIn),
    F = #feature{
        name = Name,
        boolean = Boolean,
        rollout = R,
        user_specs = US
    },
    ok = validate_feature(F),
    gen_server:call(?MODULE, {set_feature, F}).

get_features() ->
    AllFeatures = get_boolean_features_pl(),
    FeatureMaps = feature_tuples_to_maps(AllFeatures),
    FeatureMaps.

refresh_from_store() ->
    ?LOG_DEBUG(#{what => <<"Refresh from store">>}),
    gen_server:cast(?MODULE, load_from_store).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(StoreLib) ->
    start_link(StoreLib, []).

start_link(StoreLib, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [StoreLib, Opts], []).

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
init([StoreLib, Opts]) ->
    ?FEATURE_REGISTRY = ets:new(
        ?FEATURE_REGISTRY,
        [
            set,
            named_table,
            {read_concurrency, true},
            {keypos, #feature.name}
        ]
    ),
    gen_server:cast(?MODULE, load_from_store),
    StoreLibState = init_store_lib(StoreLib),
    RefreshInterval = proplists:get_value(refresh_interval, Opts, undefined),

    {ok, #state{
        store_lib_state = StoreLibState,
        refresh_interval = RefreshInterval
    }}.

init_store_lib(StoreLib) ->
    features_store_lib:init(StoreLib, "features_store").

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
handle_call({set_feature, Feature = #feature{}}, _From, State) ->
    ok = store_features([Feature]),
    {Resp, NewState} = store_in_storelib(State),
    {reply, Resp, NewState}.

boolean_to_atom(enabled) ->
    true;
boolean_to_atom(true) ->
    true;
boolean_to_atom(<<"true">>) ->
    true;
boolean_to_atom(<<"enabled">>) ->
    true;
boolean_to_atom(undefined) ->
    false;
boolean_to_atom(disabled) ->
    false;
boolean_to_atom(false) ->
    false;
boolean_to_atom(<<"false">>) ->
    false;
boolean_to_atom(<<"disabled">>) ->
    false.

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
handle_cast(
    load_from_store,
    State = #state{
        refresh_interval = RefreshInterval,
        store_lib_state = StoreLibState
    }
) ->
    {Data, NewStoreLibState} = features_store_lib:get(StoreLibState),
    LoadedState = State#state{store_lib_state = NewStoreLibState},
    AllFeatures = maps:get(feature_maps, Data, []),
    store_features(AllFeatures),

    trigger_refresh_get(RefreshInterval),
    {_StoreResp, ReadyState} = store_in_storelib(LoadedState),

    {noreply, ReadyState};
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
store_features([]) ->
    ok;
store_features([FeatureMapIn | T]) when is_map(FeatureMapIn) ->
    FeatureMap = ensure_keys_are_atoms(FeatureMapIn),
    R = #rollout_spec{
        start = sanitize_rollout_value(maps:get(rollout_start, FeatureMap)),
        'end' = sanitize_rollout_value(maps:get(rollout_end, FeatureMap))
    },
    US = build_user_specs(maps:get(user, FeatureMap)),
    F = #feature{
        name = maps:get(name, FeatureMap),
        boolean = maps:get(boolean, FeatureMap),
        rollout = R,
        user_specs = US
    },
    store_features([F | T]);
store_features([Feature = #feature{} | T]) ->
    true = ets:insert(?FEATURE_REGISTRY, Feature),

    ?LOG_DEBUG(#{
        what => feature_stored,
        module => ?MODULE,
        feature => Feature#feature.name,
        boolean => Feature#feature.boolean,
        rollout_start => Feature#feature.rollout#rollout_spec.start,
        rollout_end => Feature#feature.rollout#rollout_spec.'end',
        user => Feature#feature.user_specs
    }),

    store_features(T).

store_in_storelib(State = #state{store_lib_state = StoreLibState}) ->
    AllFeatures = get_boolean_features_pl(),
    FeatureMaps = feature_tuples_to_maps(AllFeatures),
    Data = #{feature_maps => FeatureMaps},
    {Resp, StoreLibState1} = features_store_lib:store(
        Data,
        StoreLibState
    ),
    {Resp, State#state{store_lib_state = StoreLibState1}}.

trigger_refresh_get(undefined) ->
    ok;
trigger_refresh_get(RefreshInterval) ->
    ?LOG_DEBUG(#{
        what => <<"Sending store refresh">>,
        refresh_interval => RefreshInterval
    }),
    {ok, _Ref} = timer:apply_after(
        RefreshInterval,
        ?MODULE,
        refresh_from_store,
        []
    ),
    ok.

get_boolean_features_pl() ->
    Objs = ets:match_object(?FEATURE_REGISTRY, #feature{_ = '_'}),
    Objs.

feature_tuples_to_maps([]) ->
    [];
feature_tuples_to_maps([Feature = #feature{} | T]) ->
    M = #{
        name => Feature#feature.name,
        rollout_start => Feature#feature.rollout#rollout_spec.start,
        rollout_end => Feature#feature.rollout#rollout_spec.'end',
        boolean => boolean_to_atom(Feature#feature.boolean),
        user => userspecs_to_lists(Feature#feature.user_specs)
    },

    [M] ++ feature_tuples_to_maps(T).

ensure_keys_are_atoms(Map) ->
    maps:fold(fun map_fold_fun_key_to_atom/3, #{}, Map).

map_fold_fun_key_to_atom(K, V, AccIn) when is_atom(K) ->
    maps:put(K, V, AccIn);
map_fold_fun_key_to_atom(K, V, AccIn) when is_binary(K) ->
    Ka = erlang:binary_to_atom(K, utf8),
    maps:put(Ka, V, AccIn).

validate_feature(Feature = #feature{}) ->
    validate_rollout(Feature).

validate_rollout(#feature{
    rollout = #rollout_spec{
        start = undefined,
        'end' = undefined
    }
}) ->
    ok;
validate_rollout(#feature{
    rollout = #rollout_spec{
        start = _RS,
        'end' = undefined
    }
}) ->
    throw({invalid_feature, "Rollout start must have an end"});
validate_rollout(#feature{
    rollout = #rollout_spec{
        start = Start,
        'end' = End
    }
}) ->
    case Start > End of
        true -> throw({invalid_feature, "Rollout start cannot be after the end"});
        false -> ok
    end.

build_user_specs(undefined) ->
    [];
build_user_specs([]) ->
    [];
build_user_specs([[UserProp, ComparatorBin, Value] | T]) when is_binary(ComparatorBin) ->
    ComparatorAtom = features:comparator_bin_to_atom(ComparatorBin),
    build_user_specs([[UserProp, ComparatorAtom, Value] | T]);
build_user_specs([[UserProp, ComparatorAtom = '=', Value] | T]) ->
    US = #user_spec{
        prop = UserProp,
        comparator_atom = ComparatorAtom,
        value = Value
    },
    [US | build_user_specs(T)];
build_user_specs([[UserProp, ComparatorAtom = 'in', Value] | T]) ->
    US = #user_spec{
        prop = UserProp,
        comparator_atom = ComparatorAtom,
        value = Value
    },
    [US | build_user_specs(T)];
build_user_specs([[_UserProp, _ComparatorAtom, _Value] | _T]) ->
    throw({invalid_feature, "User flag can only do `=`/`in` comparisons"});
build_user_specs([_H | _T]) ->
    throw({invalid_feature, "Incorrect number of terms for user flag"}).

userspecs_to_lists([]) ->
    [];
userspecs_to_lists([
    #user_spec{
        prop = Prop,
        comparator_atom = CA,
        value = Value
    }
    | T
]) ->
    Spec = [Prop, CA, Value],
    [Spec | userspecs_to_lists(T)].

sanitize_rollout_value(<<"undefined">>) -> undefined;
sanitize_rollout_value(undefined) -> undefined;
sanitize_rollout_value(Val) when is_integer(Val) -> Val.
