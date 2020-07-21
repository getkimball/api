-module(features_store_lib).

-export([init/2,
         store/2,
         get/1]).

-type callback_state() :: any().
-type lib_data() :: list(map()).
-type name() :: string().

-callback init(name()) -> callback_state().
-callback get_all(callback_state()) -> {lib_data(), callback_state()}.
-callback store(lib_data(), callback_state()) ->
            {ok, callback_state()} |
            {not_suported, callback_state()}.

-record(state, {mod=undefined,
                mod_state=undefined}).


init(Mod, Name) ->
    ModState = Mod:init(Name),
    #state{mod=Mod, mod_state=ModState}.

store(Data, State=#state{mod=Mod, mod_state=ModState}) ->
    {Status, ModState1} = Mod:store(Data, ModState),
    {Status, State#state{mod_state=ModState1}}.

get(State=#state{mod=Mod, mod_state=ModState}) ->
    {Data, ModState1} = Mod:get_all(ModState),
    {Data, State#state{mod_state=ModState1}}.
