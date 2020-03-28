-module(hellerl_world_handler_incoming).
-behaviour(cowboy_route_setup).

-include_lib("kernel/include/logger.hrl").

-export([init/2,
         setup/0]).

-define(M_INCOMING_LOGS_HANDLED, incoming_logs_handled).

setup() ->
    prometheus_counter:new([{name, ?M_INCOMING_LOGS_HANDLED},
                            {help, "Number of log messages"}]),
    #{routes => [{"/incoming", ?MODULE, []}]}.


init(Req0 = #{method := <<"POST">>,
              headers := #{<<"content-type">> := <<"application/json">>}},
            Opts) ->
    {ok, DataBin, Req1} = cowboy_req:read_body(Req0),
    Data = jsx:decode(DataBin, [return_maps]),
    Len = length(Data),
    ?LOG_INFO(#{what => "Incoming log message received",
            data => Data,
            length => Len}),
    Rep = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"POST">> , Req1),
    prometheus_counter:inc(?M_INCOMING_LOGS_HANDLED, [], Len),
    {ok, Rep, Opts};
init(Req0, Opts) ->
    Msg = <<"Requests should be posted with content-type: application/json">>,
    Rep = cowboy_req:reply(400, #{
        <<"content-type">> => <<"text/plain">>
    }, Msg, Req0),

    {ok, Rep, Opts}.
