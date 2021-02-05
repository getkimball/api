-module(features_grpc_sup).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).
-export([init/1]).
-export([start_relay/2]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?LOG_INFO(#{what => <<"gRPC Supervisor starting">>}),
    Procs = [],
    Flags = #{
        strategy => rest_for_one,
        intensity => 0,
        period => 1
    },

    {ok, {Flags, Procs}}.

start_relay(Host, Port) ->
    Spec = #{
        id => {Host, Port},
        start => {features_grpc_relay, start_link, [Host, Port]}
    },
    Info = supervisor:start_child(?MODULE, Spec),
    Pid = pid_from_child_start(Info),
    features_grpc_gen_event_forwarder:add_grpc_relay(Pid),
    ok.

pid_from_child_start({_, Pid}) when is_pid(Pid) ->
    Pid;
pid_from_child_start({_, {_, Pid}}) when is_pid(Pid) ->
    Pid.
