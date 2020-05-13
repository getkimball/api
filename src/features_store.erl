-module(features_store).
-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).


%% Library behavior
-type callback_state() :: any().
-type lib_data() :: list(map()).

-callback init() -> callback_state().
-callback get_all(callback_state()) -> {lib_data(), callback_state()}.
-callback store(lib_data(), callback_state()) -> callback_state().

%%

%% API functions
-export([start_link/0,
         start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([set_binary_feature/2,
         get_binary_feature/1,
         get_binary_features/0]).

-record(state, {store_lib=undefined,
                store_lib_state=undefined}).
-define(FEATURE_REGISTRY, feature_registry_table).

%%%===================================================================
%%% API functions
%%%===================================================================

set_binary_feature(Name, Status) ->
    gen_server:call(?MODULE, {set_binary_feature, Name, Status}).

get_binary_feature(Name) ->
    [{Name, AtomStatus}] = ets:lookup(?FEATURE_REGISTRY, Name),
    AtomStatus.

get_binary_features() ->
    Objs = get_binary_features_pl(),
    M = maps:from_list(Objs),
    M.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(undefined).

start_link(StoreLib) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, StoreLib, []).

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
init(StoreLib) ->
    ?FEATURE_REGISTRY = ets:new(?FEATURE_REGISTRY,
                                [set, named_table, {read_concurrency, true}]),
    gen_server:cast(?MODULE, load_from_store),
    StoreLibState = init_store_lib(StoreLib),
    {ok, #state{store_lib=StoreLib,
                store_lib_state=StoreLibState}}.

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
handle_call({set_binary_feature, Name, Status}, _From, State) ->
    ok = store_features([#{name=>Name, status=>Status}]),
    NewState = store_in_storelib(State),
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

status_to_atom(<<"enabled">>) ->
    enabled;
status_to_atom(<<"disabled">>) ->
    disabled.

atom_to_status(enabled) ->
    <<"enabled">>;
atom_to_status(disabled) ->
    <<"disabled">>.


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
handle_cast(load_from_store, State=#state{store_lib=StoreLib,
                                          store_lib_state=StoreLibState}) ->
    {AllFeatures, NewStoreLibState} = StoreLib:get_all(StoreLibState),
    store_features(AllFeatures),
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
store_features([#{<<"name">> := Name, <<"status">> := Status} | T]) ->
    store_features([#{name=>Name, status=>Status} | T]);
store_features([#{name := Name, status := Status} | T]) ->
    AtomStatus = status_to_atom(Status),

    true = ets:insert(?FEATURE_REGISTRY, {Name, AtomStatus}),

    ?LOG_DEBUG(#{what=>feature_stored,
                 module=>?MODULE,
                 feature=>Name,
                 status=>AtomStatus
    }),

    store_features(T).

store_in_storelib(State=#state{store_lib=undefined}) ->
    State;
store_in_storelib(State=#state{store_lib=StoreLib,
                               store_lib_state=StoreLibState}) ->
    AllFeatures = get_binary_features_pl(),
    FeatureMaps = features_pl_to_maps(AllFeatures),
    NewStoreLibState = StoreLib:store(FeatureMaps, StoreLibState),
    State#state{store_lib_state=NewStoreLibState}.

get_binary_features_pl() ->
    Objs = ets:match_object(?FEATURE_REGISTRY, {'$0', '$1'}),
    Objs.

features_pl_to_maps([]) ->
    [];
features_pl_to_maps([{Name, Status}|T]) when is_atom(Status) ->
    M = #{name=>Name, status=>atom_to_status(Status)},
    [M] ++ features_pl_to_maps(T).
