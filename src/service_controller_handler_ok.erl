-module(service_controller_handler_ok).
-behaviour(cowboy_route_setup).

-export([init/2,
         setup/0]).

-include_lib("kernel/include/logger.hrl").

setup() ->
    #{routes => [{"/ready", ?MODULE, []},
                 {"/alive", ?MODULE, []}]}.

init(Req0, Opts) ->
    HAPFrontends = haproxy:frontends(),
    ?LOG_DEBUG(#{what=><<"frontends">>,
                frontends=>HAPFrontends}),

    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"ok!">>, Req0),


    {ok, Req, Opts}.
