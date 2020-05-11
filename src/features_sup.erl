-module(features_sup).
-behaviour(supervisor).
-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?LOG_INFO(#{what=><<"Supervisor starting">>}),

    Procs = [],
    {ok, {{one_for_one, 1, 5}, Procs}}.
