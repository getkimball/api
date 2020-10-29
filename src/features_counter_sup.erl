-module(features_counter_sup).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).
-export([init/1]).

start_link(StoreLib) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [StoreLib]).

init([StoreLib]) ->
    ?LOG_INFO(#{what => <<"Counter Supervisor starting">>}),
    Procs = [
        #{
            id => features_count_router,
            start => {features_count_router, start_link, [StoreLib]}
        },
        #{
            id => features_counter_persist_manager,
            start => {features_counter_persist_manager, start_link, []}
        }
    ],
    Flags = #{
        strategy => rest_for_one,
        intensity => 0,
        period => 1
    },

    {ok, {Flags, Procs}}.
