%%%-------------------------------------------------------------------
%%% @copyright 2020 Get Kimball Inc.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(features_counter).
-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

%% API functions
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([add/2,
         count/1,
         persist/1]).

-record(state, {name=undefined,
                store_lib_state=undefined,
                unpersisted_write=false,
                bloom=undefined}).

%%%===================================================================
%%% API functions
%%%===================================================================
add(Key, Pid) when is_integer(Key) ->
    KeyBin = list_to_binary(integer_to_list(Key)),
    add(KeyBin, Pid);
add(Key, Pid) when is_binary(Key), is_pid(Pid) ->
    gen_server:cast(Pid, {add, Key}).

count(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, count).

persist(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, persist).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(StoreLib, Name) ->
    gen_server:start_link(?MODULE, [StoreLib, Name], []).

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
init([StoreLib, Name]) ->
    ?LOG_DEBUG(#{what=><<"features_counter starting">>,
                 name=>Name}),
    register_name(Name),
    StoreLibState = features_store_lib:init(StoreLib, {"counter", Name}),
    gen_server:cast(self(), load_or_init),
    {ok, _TRef} = timer:apply_interval(15000, ?MODULE, persist, [self()]),
    {ok, #state{name=Name,
                store_lib_state=StoreLibState,
                bloom=undefined}}.

register_name([]) ->
    ok;
register_name(Name) ->
    features_count_router:register_counter(Name, self()).

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
handle_call(count, _From, State=#state{bloom=Bloom}) ->
    Size = etbloom:size(Bloom),
    Reply = Size,
    {reply, Reply, State};
handle_call(persist, _From, State=#state{name=Name, unpersisted_write=false}) ->
    ?LOG_DEBUG(#{what=><<"features_counter persist">>,
                 needs_to_persist=>false,
                 name=>Name}),
    {reply, ok, State};
handle_call(persist, _From, State=#state{store_lib_state=StoreLibState,
                                         name=Name,
                                         bloom=Bloom}) ->
    ?LOG_DEBUG(#{what=><<"features_counter persist">>,
                 needs_to_persist=>true,
                 name=>Name}),
    {Status, StoreLibState1} = store(Bloom, StoreLibState),
    Reply = Status,
    {reply, Reply, State#state{store_lib_state=StoreLibState1,
                               unpersisted_write=false}}.

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
handle_cast({add, Key}, State=#state{name=Name,
                                     bloom=Bloom}) ->
    NewBloom = etbloom:add(Key, Bloom),
    Size = etbloom:size(NewBloom),

    ?LOG_DEBUG(#{what=><<"features_counter add">>,
                 name=>Name,
                 size=>Size,
                 key=>Key}),
    {noreply, State#state{bloom=NewBloom,
                          unpersisted_write=true}};
handle_cast(load_or_init, State=#state{store_lib_state=StoreLibState}) ->
    {Data, StoreLibState1} = features_store_lib:get(StoreLibState),
    Bloom = bloom_filter_from_data(Data),
    {noreply, State#state{bloom=Bloom,
                          store_lib_state=StoreLibState1}}.

bloom_filter_from_data(#{bloom:=Bloom}) ->
    Bloom;
bloom_filter_from_data(_Else) ->
    InitialSize = 1000000,
    Bloom = etbloom:sbf(InitialSize),
    Bloom.

store(Bloom, State) ->
    Data = #{bloom => Bloom},
    features_store_lib:store(Data, State).
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
