-module(features_store).
-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).


%% Library behavior
-type callback_state() :: any().
-type lib_data() :: list(map()).

-callback init() -> callback_state().
-callback get_all(callback_state()) -> {lib_data(), callback_state()}.
-callback store(lib_data(), callback_state()) ->
            {ok, callback_state()} |
            {not_suported, callback_state()}.

%%

%% API functions
-export([start_link/0,
         start_link/1,
         start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([set_feature/3,
         get_features/0,
         refresh_from_store/0]).

-record(state, {refresh_interval=undefined,
                store_lib=undefined,
                store_lib_state=undefined}).

-record(feature, {name=undefined,
                  boolean=undefined}).

-define(FEATURE_REGISTRY, feature_registry_table).

%%%===================================================================
%%% API functions
%%%===================================================================

set_feature(Name, boolean, Boolean) ->
    gen_server:call(?MODULE, {set_feature, Name, boolean, Boolean}).

get_features() ->
    Objs = get_boolean_features_pl(),
    M = feature_tuples_to_single_map(Objs),
    M.

refresh_from_store() ->
    ?LOG_DEBUG(#{what=><<"Refresh from store">>}),
    gen_server:cast(?MODULE, load_from_store).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(undefined, []).

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
    ?FEATURE_REGISTRY = ets:new(?FEATURE_REGISTRY,
                                [set,
                                 named_table,
                                 {read_concurrency, true},
                                 {keypos, #feature.name}]),
    gen_server:cast(?MODULE, load_from_store),
    StoreLibState = init_store_lib(StoreLib),
    RefreshInterval = proplists:get_value(refresh_interval, Opts, undefined),

    {ok, #state{store_lib=StoreLib,
                store_lib_state=StoreLibState,
                refresh_interval=RefreshInterval}}.

init_store_lib(undefined) ->
    undefined;
init_store_lib(StoreLib) ->
    StoreLib:init().

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
handle_call({set_feature, Name, boolean, Boolean}, _From, State) ->
    ok = store_features([#{name=>Name, boolean=>Boolean}]),
    {Resp, NewState} = store_in_storelib(State),
    {reply, Resp, NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

boolean_to_atom(enabled) ->
    true;
boolean_to_atom(true) ->
    true;
boolean_to_atom(<<"true">>) ->
    true;
boolean_to_atom(disabled) ->
    false;
boolean_to_atom(false) ->
    false;
boolean_to_atom(<<"false">>) ->
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
handle_cast(load_from_store, State=#state{store_lib=undefined}) ->
    {noreply, State};
handle_cast(load_from_store, State=#state{refresh_interval=RefreshInterval,
                                          store_lib=StoreLib,
                                          store_lib_state=StoreLibState}) ->
    {AllFeatures, NewStoreLibState} = StoreLib:get_all(StoreLibState),
    store_features(AllFeatures),

    trigger_refresh_get(RefreshInterval),

    {noreply, State#state{store_lib_state=NewStoreLibState}};
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
store_features([#{<<"name">> := Name, <<"boolean">> := Boolean} | T]) ->
    store_features([#{name=>Name, boolean=>Boolean} | T]);
store_features([#{name := Name, boolean := Boolean} | T]) ->
    R = #feature{name=Name, boolean=Boolean},
    true = ets:insert(?FEATURE_REGISTRY, R),

    ?LOG_DEBUG(#{what=>feature_stored,
                 module=>?MODULE,
                 feature=>Name,
                 boolean=>Boolean
    }),

    store_features(T).

store_in_storelib(State=#state{store_lib=undefined}) ->
    {ok, State};
store_in_storelib(State=#state{store_lib=StoreLib,
                               store_lib_state=StoreLibState}) ->
    AllFeatures = get_boolean_features_pl(),
    FeatureMaps = feature_tuples_to_maps(AllFeatures),
    {Resp, NewStoreLibState} = StoreLib:store(FeatureMaps, StoreLibState),
    {Resp, State#state{store_lib_state=NewStoreLibState}}.


trigger_refresh_get(undefined) ->
    ok;
trigger_refresh_get(RefreshInterval) ->
    ?LOG_DEBUG(#{what=><<"Sending store refresh">>,
                 refresh_interval=>RefreshInterval}),
    {ok, _Ref} = timer:apply_after(
        RefreshInterval, ?MODULE, refresh_from_store, []),
    ok.

get_boolean_features_pl() ->
    Objs = ets:match_object(?FEATURE_REGISTRY, #feature{_='_'}),
    Objs.

feature_tuples_to_maps([]) ->
    [];
feature_tuples_to_maps([Feature=#feature{}|T]) ->
    M = #{name=>Feature#feature.name,
          boolean=>Feature#feature.boolean},
    [M] ++ feature_tuples_to_maps(T).

feature_tuples_to_single_map(Tup) ->
    feature_tuples_to_single_map(Tup, #{}).

feature_tuples_to_single_map([], M) ->
    M;
feature_tuples_to_single_map([Feature=#feature{}|T], M) ->
    NewM = maps:put(Feature#feature.name,
                    boolean_to_atom(Feature#feature.boolean), M),
    feature_tuples_to_single_map(T, NewM).
