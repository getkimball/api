%% Feel free to use, reuse and abuse the code in this file.

-module(service_controller_hap_backend_decider).
-include_lib("kernel/include/logger.hrl").
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).

start_link(Ref, _Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts = []) ->
    {ok, Socket} = ranch:handshake(Ref),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    ?LOG_DEBUG(#{what=>"Backend decision request"}),
    case Transport:recv(Socket, 0, 100) of
      {ok, Data} when Data =/= <<4>> ->
        {ok, FEName} = fe_name(Data),
        Backends = service_controller_service_watcher:backends(FEName),
        BEChoice = lists:nth(rand:uniform(length(Backends)), Backends),
        ?LOG_DEBUG(#{what=>"Backend decision request",
                     frontend_name=>FEName,
                     backends=>Backends,
                     backend_choice=>BEChoice,
                     data=>Data}),
        Transport:send(Socket, BEChoice),
        ok = Transport:close(Socket);
      _ ->
        ?LOG_DEBUG(#{what=>"Backend decision request closed"}),
        ok = Transport:close(Socket)
    end.

fe_name(Bin) ->
    [FEName, _Rest] = binary:split(Bin, [<<"\n">>], [global]),
    {ok, FEName}.
