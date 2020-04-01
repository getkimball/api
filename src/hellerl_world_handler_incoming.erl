-module(hellerl_world_handler_incoming).
-behaviour(cowboy_route_setup).

-include_lib("kernel/include/logger.hrl").

-export([init/2,
         setup/0]).

-define(M_INCOMING_LOGS_HANDLED, incoming_logs_handled).
-define(M_LOGS_PER_APP, incoming_logs_handled_for_app).
-define(M_LOGS_WITHOUT_KUBE_APP, incoming_logs_non_kubernetes).
-define(TRACKED_LABELS,
        [<<"namespace">>, <<"container_name">>, <<"user_action">>]).

setup() ->
    ok = prometheus_counter:new([{name, ?M_INCOMING_LOGS_HANDLED},
                                 {help, "Number of log messages"}]),


    MetricSpec0 = [{name, ?M_LOGS_PER_APP},
                   {help, "Number of user actions for an app"},
                   {labels, ?TRACKED_LABELS}],
    ok = prometheus_counter:new(MetricSpec0),

    MetricSpec1 = [{name, ?M_LOGS_WITHOUT_KUBE_APP},
                   {help, "Number of log messages not from Kube"}],
    ok = prometheus_counter:new(MetricSpec1),

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
    ok = process_logs(Data),
    {ok, Rep, Opts};
init(Req0, Opts) ->
    Msg = <<"Requests should be posted with content-type: application/json">>,
    Rep = cowboy_req:reply(400, #{
        <<"content-type">> => <<"text/plain">>
    }, Msg, Req0),

    {ok, Rep, Opts}.

process_logs([]) ->
    ok;
process_logs([H = #{<<"kubernetes">> := _Any}|T]) ->
    Values = gka_extract_data_from_log:label_values(?TRACKED_LABELS, H),
    prometheus_counter:inc(?M_LOGS_PER_APP, Values),

    process_logs(T);
process_logs([_H|T]) ->
    prometheus_counter:inc(?M_LOGS_WITHOUT_KUBE_APP),
    process_logs(T).
