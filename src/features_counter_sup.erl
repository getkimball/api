-module(features_counter_sup).
-behaviour(supervisor).
-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?LOG_INFO(#{what=><<"Counter Supervisor starting">>}),
    Procs = [
        #{id => features_counter,
          start => {features_counter, start_link, []}}
    ],

    {ok, {{simple_one_for_one, 1, 5}, Procs}}.
