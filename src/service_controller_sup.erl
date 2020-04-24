-module(service_controller_sup).
-behaviour(supervisor).
-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?LOG_INFO(#{what=><<"Supervisor starting">>}),
    ?LOG_INFO(#{what=>"Getting Kubernetes API Credentials"}),

    Operations = [
        <<"listCoreV1ServiceForAllNamespaces">>
    ],

    API = kuberlnetes:load([{operations, Operations}]),
    Ops = swaggerl:operations(API),
    ?LOG_INFO(#{what=>"Kubernetes Operations",
                ops=>Ops}),
    Procs = [],
    {ok, {{one_for_one, 1, 5}, Procs}}.
