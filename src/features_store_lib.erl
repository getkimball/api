-module(features_store_lib).

-export([init/2,
         store/2,
         get/1]).

-type callback_state() :: any().
-type lib_data() :: map().
-type name() :: string()| {string(), string()}.
-type counter_id() :: tuple().

-callback init(name()|counter_id()) -> callback_state().
-callback get_all(callback_state()) -> {lib_data(), callback_state()}.
-callback store(lib_data(), callback_state()) ->
            {ok, callback_state()} |
            {not_supported, callback_state()}.

-record(state, {mod=undefined,
                mod_state=undefined}).


init(undefined, _Name) ->
    #state{mod=undefined};
init(Mod, Name) ->
    ModState = Mod:init(Name),
    #state{mod=Mod, mod_state=ModState}.

store(_Data, State=#state{mod=undefined}) ->
    {not_supported, State};
store(Data, State=#state{mod=Mod, mod_state=ModState}) ->
    {Status, ModState1} = Mod:store(Data, ModState),
    {Status, State#state{mod_state=ModState1}}.

get(State=#state{mod=undefined}) ->
    {not_supported, State};
get(State=#state{mod=Mod, mod_state=ModState}) ->
    {Data, ModState1} = Mod:get_all(ModState),
    {Data, State#state{mod_state=ModState1}}.
