-module(service_controller_sup).
-behaviour(supervisor).
-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    ?LOG_INFO(#{what=><<"Supervisor starting">>}),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [],
    {ok, {{one_for_one, 1, 5}, Procs}}.
