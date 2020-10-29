-module(features_sup).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start_link/3]).
-export([init/1]).

start_link(Mode, StoreLib, MetricsOpts) ->
    supervisor:start_link(
        {local, ?MODULE},
        ?MODULE,
        [Mode, StoreLib, MetricsOpts]
    ).

init([Mode, StoreLib, MetricsOpts]) ->
    ?LOG_INFO(#{
        what => <<"Supervisor starting">>,
        mode => Mode,
        store_lib => StoreLib
    }),
    Procs =
        case Mode of
            api_server ->
                [
                    #{
                        id => features_store,
                        start => {features_store, start_link, [features_store_lib_configmap]}
                    },
                    #{
                        id => features_counter_sup,
                        type => supervisor,
                        start => {features_counter_sup, start_link, [StoreLib]}
                    }
                ];
            sidecar ->
                [
                    #{
                        id => features_store,
                        start =>
                            {features_store, start_link, [
                                features_store_lib_file,
                                [{refresh_interval, 15000}]
                            ]}
                    }
                ]
        end,

    AlwaysProcs = [
        #{
            id => metrics_server,
            start => {metrics_server, start_link, [MetricsOpts]}
        }
    ],

    {ok, {{one_for_one, 1, 5}, Procs ++ AlwaysProcs}}.
