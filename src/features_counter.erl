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
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    add/2,
    add/3,
    add/4,
    count/1,
    includes_key/2,
    persist/1
]).

-record(state, {
    id = undefined,
    store_lib_state = undefined,
    unpersisted_write = false,
    tag_counts = #{},
    single_tag_counts = #{},
    bloom = undefined,
    value = #{}
}).

-define(PROM_COUNTER_REGISTRY, counters).

%%%===================================================================
%%% API functions
%%%===================================================================
add(Key, Pid) when is_integer(Key) ->
    KeyBin = list_to_binary(integer_to_list(Key)),
    add(KeyBin, Pid);
add(Key, Pid) when is_binary(Key), is_pid(Pid) ->
    Key2 = binary:copy(Key),
    Tags = [],
    Value = undefined,
    gen_server:cast(Pid, {add, Key2, Tags, Value}).

add(Key, Tags, Pid) when is_binary(Key), is_list(Tags), is_pid(Pid) ->
    Key2 = binary:copy(Key),
    Value = undefined,
    gen_server:cast(Pid, {add, Key2, Tags, Value});
add(Key, Value, Pid) when is_integer(Key) ->
    KeyBin = list_to_binary(integer_to_list(Key)),
    add(KeyBin, Value, Pid);
add(Key, Value, Pid) when is_binary(Key), is_pid(Pid) ->
    Tags = [],
    gen_server:cast(Pid, {add, Key, Tags, Value}).

add(Key, Tags, Value, Pid) when is_binary(Key) ->
    gen_server:cast(Pid, {add, Key, Tags, Value}).

count(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, count).

includes_key(Key, Pid) when is_binary(Key), is_pid(Pid) ->
    gen_server:call(Pid, {includes_key, Key}).

persist(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, persist).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(StoreLib, ID) ->
    gen_server:start_link(?MODULE, [StoreLib, ID], []).

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
init([StoreLib, ID]) ->
    ?LOG_DEBUG(#{
        what => <<"features_counter starting">>,
        id => ID
    }),
    StoreLibState = features_store_lib:init(StoreLib, {"counter", ID}),
    gen_server:cast(self(), load_or_init),

    declare_prometheus_gauge(ID),
    register_id(ID),
    {ok, #state{
        id = ID,
        store_lib_state = StoreLibState,
        bloom = undefined
    }}.

register_id(ID) ->
    features_count_router:register_counter(ID, self()).

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
handle_call(
    count,
    _From,
    State = #state{
        bloom = Bloom,
        single_tag_counts = STC,
        tag_counts = TagCounts,
        value = Value
    }
) ->
    Size = etbloom:size(Bloom),
    Reply = #{
        count => Size,
        single_tag_counts => STC,
        tag_counts => TagCounts,
        value => Value
    },
    {reply, Reply, State};
handle_call({includes_key, Key}, _From, State = #state{bloom = Bloom}) ->
    Included = etbloom:member(Key, Bloom),
    {reply, Included, State};
handle_call(persist, _From, State = #state{id = ID, unpersisted_write = false}) ->
    ?LOG_DEBUG(#{
        what => <<"features_counter persist">>,
        needs_to_persist => false,
        id => ID
    }),
    {reply, ok, State};
handle_call(
    persist,
    _From,
    State = #state{
        store_lib_state = StoreLibState,
        id = ID
    }
) ->
    ?LOG_DEBUG(#{
        what => <<"features_counter persist">>,
        needs_to_persist => true,
        id => ID
    }),
    {Status, StoreLibState1} = store(StoreLibState, State),
    Reply = Status,
    {reply, Reply, State#state{
        store_lib_state = StoreLibState1,
        unpersisted_write = false
    }}.

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
    {add, Key, Tags, AddValue},
    State = #state{
        id = ID,
        bloom = Bloom,
        single_tag_counts = STC,
        tag_counts = TagCounts,
        value = Value,
        unpersisted_write = UnpersistedWrite
    }
) ->
    InitialSize = etbloom:size(Bloom),
    NewBloom = etbloom:add(Key, Bloom),
    NewSize = etbloom:size(NewBloom),

    SortedTags = lists:usort(Tags),

    IsNewWrite = InitialSize /= NewSize,
    ShouldPersist = UnpersistedWrite or IsNewWrite,
    NewValue = update_value(IsNewWrite, AddValue, Value),

    {NewTagCounts, STC1} =
        case IsNewWrite of
            false ->
                {TagCounts, STC};
            true ->
                {increment_tag_count(SortedTags, TagCounts),
                    increment_single_tag_counts(SortedTags, STC)}
        end,

    ok = set_prometheus_gauge(IsNewWrite, ID, NewSize),

    ?LOG_DEBUG(#{
        what => <<"features_counter add">>,
        id => ID,
        size => NewSize,
        tags => SortedTags,
        should_persist => ShouldPersist,
        is_new_write => IsNewWrite,
        initial_filter_size => InitialSize,
        new_filter_size => NewSize,
        new_value => NewValue,
        key => Key
    }),
    {noreply, State#state{
        bloom = NewBloom,
        tag_counts = NewTagCounts,
        single_tag_counts = STC1,
        value = NewValue,
        unpersisted_write = ShouldPersist
    }};
handle_cast(
    load_or_init,
    State = #state{
        store_lib_state = StoreLibState,
        single_tag_counts = STC,
        tag_counts = TagCounts,
        value = Value
    }
) ->
    {LoadedData, StoreLibState1} = features_store_lib:get(StoreLibState),
    Data =
        case LoadedData of
            not_supported -> #{};
            Else -> Else
        end,

    Bloom = bloom_filter_from_data(Data, State),
    LoadedSTC = maps:get(single_tag_counts, Data, STC),
    LoadedTagCounts = maps:get(tag_counts, Data, TagCounts),
    LoadedValue = maps:get(value, Data, Value),

    {noreply, State#state{
        bloom = Bloom,
        single_tag_counts = LoadedSTC,
        store_lib_state = StoreLibState1,
        tag_counts = LoadedTagCounts,
        value = LoadedValue
    }}.

bloom_filter_from_data(#{bloom := Bloom}, _State) ->
    Bloom;
bloom_filter_from_data(_Else, #state{id = ID}) ->
    Name = features_counter_id:name(ID),
    features_counter_config:create_bloom(Name).

store(
    StoreLibState,
    _State = #state{
        bloom = Bloom,
        single_tag_counts = STC,
        tag_counts = TagCounts,
        value = Value
    }
) ->
    Data = #{
        bloom => Bloom,
        single_tag_counts => STC,
        tag_counts => TagCounts,
        value => Value
    },
    features_store_lib:store(Data, StoreLibState).

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
increment_tag_count(Tags, TagCounts) ->
    Count = maps:get(Tags, TagCounts, 0),
    NewCounts = maps:put(Tags, Count + 1, TagCounts),
    NewCounts.

increment_single_tag_counts(Tags, TagCounts) ->
    Incr = fun(T, AccIn) ->
        Val = maps:get(T, AccIn, 0),
        AccIn#{T => Val + 1}
    end,
    lists:foldl(Incr, TagCounts, Tags).

declare_prometheus_gauge(ID) ->
    prometheus_gauge:declare([
        {name, features_counter_id:to_prometheus_name(ID)},
        {help, "Value of event counters"},
        {labels, features_counter_id:to_prometheus_label_keys(ID)},
        {registry, ?PROM_COUNTER_REGISTRY}
    ]).

set_prometheus_gauge(false, _ID, _Size) ->
    ok;
set_prometheus_gauge(true, ID, Size) ->
    prometheus_gauge:set(
        ?PROM_COUNTER_REGISTRY,
        features_counter_id:to_prometheus_name(ID),
        features_counter_id:to_prometheus_label_values(ID),
        Size
    ).

update_value(false, _AddValue, Value) ->
    Value;
update_value(_, undefined, Value) ->
    Value;
update_value(true, AddValue, Value) ->
    Sum = maps:get(sum, Value, 0),
    NewSum = AddValue + Sum,
    #{sum => NewSum}.
