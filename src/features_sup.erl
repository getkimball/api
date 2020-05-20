-module(features_sup).
-behaviour(supervisor).
-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).
-export([init/1]).

start_link(Mode) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Mode]).

init([Mode]) ->
    ?LOG_INFO(#{what=><<"Supervisor starting">>}),
    Procs = case Mode of
       api_server -> [
        #{id    => features_store,
          start => {features_store, start_link, [features_store_lib_configmap]}
        }];
       sidecar -> [
        #{id    => features_store,
          start => {features_store, start_link, [features_store_lib_file,
                                                 [{refresh_interval, 15000}]]}
        }]
    end,

    {ok, {{one_for_one, 1, 5}, Procs}}.
