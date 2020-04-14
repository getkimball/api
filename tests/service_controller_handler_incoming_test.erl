-module(service_controller_handler_incoming_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, service_controller_handler_incoming).

setup_test() ->
    {ok, _} = application:ensure_all_started(prometheus),
    Setup = ?MUT:setup(),
    Routes = maps:get(routes, Setup),

    ?assert(lists:member({"/incoming", ?MUT, []}, Routes)),

    ok.
