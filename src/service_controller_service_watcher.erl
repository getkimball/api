-module(service_controller_service_watcher).
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
         backends/1,
         code_change/3]).

-record(state, {api, pid}).

-define(SERVICE_REGISTRY, service_registry_table).

%%%===================================================================
%%% API functions
%%%===================================================================
backends(Frontend) ->
    Registrations = ets:lookup(?SERVICE_REGISTRY, Frontend),
    [BE || {_FE, BE} <- Registrations].


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

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
init(API) ->
    ?LOG_INFO(#{msg => "Service watcher starting"}),
    Self = self(),
    ?SERVICE_REGISTRY = ets:new(?SERVICE_REGISTRY,
                                [bag, named_table, {read_concurrency, true}]),
    Callback = fun({Type, Obj}) -> Self ! {kubewatch, Type, Obj} end,
    Pid = kuberlnetes:spawn_watch(
        Callback, API, "listCoreV1ServiceForAllNamespaces", []),
    {ok, #state{api=API, pid=Pid}}.

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
handle_info({kubewatch, Type= <<"DELETED">>, Object =
                  #{<<"metadata">> :=
                    #{<<"namespace">> := Namespace,
                      <<"name">> := Name ,
                      <<"annotations">> :=
                      #{<<"getkimball.com/name">> := GKName,
                        <<"getkimball.com/enabled">> := <<"true">>}},
                    <<"spec">> := #{
                      <<"clusterIP">> := ClusterIP,
                      <<"ports">> := ServicePorts
                  }}},
            State) ->
    ?LOG_INFO(#{what=><<"Watched object">>,
                type=>Type,
                namespace=>Namespace,
                cluster_ip=>ClusterIP,
                ports=>ServicePorts,
                gkname=>GKName,
                name=>Name}),
    ?LOG_DEBUG(#{what=><<"Watched object">>,
                type=>Type,
                gkname=>GKName,
                object=>Object}),
    %% TODO: Get port number by name from above
    FrontendName = GKName,
    BackendName = << Namespace/binary, <<"_">>/binary, Name/binary >>,
    ok = ensure_service_unregistered(FrontendName, BackendName),
    ok = haproxy:ensure_no_backend(BackendName),
    {noreply, State};

handle_info({kubewatch, Type, Object =
                  #{<<"metadata">> :=
                    #{<<"namespace">> := Namespace,
                      <<"name">> := Name ,
                      <<"annotations">> :=
                      #{<<"getkimball.com/name">> := GKName,
                        <<"getkimball.com/enabled">> := <<"true">>}},
                    <<"spec">> := #{
                      <<"clusterIP">> := ClusterIP,
                      <<"ports">> := ServicePorts
                  }}},
            State) ->
    ?LOG_INFO(#{what=><<"Watched object">>,
                type=>Type,
                namespace=>Namespace,
                cluster_ip=>ClusterIP,
                ports=>ServicePorts,
                gkname=>GKName,
                name=>Name}),
    ?LOG_DEBUG(#{what=><<"Watched object">>,
                type=>Type,
                gkname=>GKName,
                object=>Object}),
    %% TODO: Get port number by name from above
    FrontendName = GKName,
    BackendName = << Namespace/binary, <<"_">>/binary, Name/binary >>,
    ok = haproxy:ensure_backend(BackendName, #{name=>BackendName,
                                               mode=><<"http">>}),
    ok = haproxy:ensure_server(BackendName, #{backend_name=>BackendName,
                                              cluster_ip=>ClusterIP,
                                              port=>80}),
    ok = haproxy:ensure_frontend(FrontendName, #{default_backend=>BackendName}),
    ok = haproxy:ensure_bind(FrontendName, #{port=>80}),
    % ok = haproxy:ensure_gk_lua(GKName),
    ok = ensure_service_registered(FrontendName, BackendName),

    {noreply, State};
handle_info({kubewatch, Type, Object = #{<<"metadata">> :=
                                          #{<<"namespace">> := Namespace,
                                            <<"name">> := Name }}}, State) ->
    ?LOG_INFO(#{what=><<"Unwatched object">>,
                type=>Type,
                namespace=>Namespace,
                name=>Name}),
    ?LOG_DEBUG(#{what=><<"Unwatched object">>,
                type=>Type,
                object=>Object,
                namespace=>Namespace,
                name=>Name}),
    {noreply, State};
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

ensure_service_registered(Frontend, Backend) ->
    true = ets:insert(?SERVICE_REGISTRY, {Frontend, Backend}),
    ok.

ensure_service_unregistered(Frontend, Backend) ->
    true = ets:delete_object(?SERVICE_REGISTRY, {Frontend, Backend}),
    ok.
