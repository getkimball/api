%%%-------------------------------------------------------------------
%%% @copyright 2021 Get Kimball Inc.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(features_grpc_relay).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_statem).

%% API functions
-export([start_link/2]).

-export([terminate/3, code_change/4, init/1, callback_mode/0, handle_event/4]).

-export([notify/2]).

-record(data, {host, port, conn, event_stream}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Host, Port) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Host, Port], []).

notify(Pid, Msg) ->
    gen_statem:cast(Pid, {notify, Msg}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> [handle_event_function].

init([Host, Port]) ->
    try_to_connect(),
    {ok, connecting, #data{host = Host, port = Port}}.

handle_event(cast, connect, State = connecting, Data = #data{host = Host, port = Port}) ->
    NewConnectionStatus = grpc_client:connect(tcp, Host, Port),
    ?LOG_INFO(#{
        what => grpc_event_handler,
        host => Host,
        connection_status => NewConnectionStatus,
        port => Port
    }),

    handle_connect_response(NewConnectionStatus, State, Data);
handle_event(
    cast,
    connect_stream,
    connected,
    Data = #data{host = Host, port = Port, conn = Connection}
) ->
    NewStreamStatus = grpc_client:new_stream(
        Connection,
        'KimballIntegration',
        'EventStream',
        features_proto_pb
    ),
    case NewStreamStatus of
        {ok, EventStream} ->
            {next_state, stream_connected, Data#data{conn = Connection, event_stream = EventStream}};
        {error, ErrorMsg} ->
            ?LOG_INFO(#{
                what => grpc_stream_creation_error,
                host => Host,
                why => ErrorMsg,
                port => Port
            }),
            {ok, _TRef} = timer:apply_after(5000, gen_statem, cast, [self(), connect_stream]),
            {keep_state, Data}
    end;
handle_event(cast, {notify, {NS, Event, Key}}, stream_connected, Data = #data{event_stream = ES}) ->
    ?LOG_DEBUG(#{
        what => grpc_event_sending,
        event_stream => ES,
        namespace => NS,
        event => Event,
        key => Key
    }),
    ok = grpc_client:send(ES, #{
        namespace => NS,
        name => Event,
        key => Key
    }),

    ?LOG_DEBUG(#{
        what => grpc_event_sent,
        event_stream => ES,
        namespace => NS,
        event => Event,
        key => Key
    }),
    {keep_state, Data};
handle_event(cast, {notify, {NS, Event, Key}}, _State, Data = #data{host = Host, port = Port}) ->
    ?LOG_DEBUG(#{
        what => grpc_stream_missed_event,
        host => Host,
        why => <<"stream not connected">>,
        namespace => NS,
        event => Event,
        key => Key,
        port => Port
    }),
    {keep_state, Data};
handle_event(info, {'EXIT', _HTTPPid, econnrefused}, _State, Data) ->
    {next_state, connecting, Data#data{conn = undefined, event_stream = undefined}};
handle_event(info, {'EXIT', _HTTPPid, closed_by_peer}, stream_connected, Data) ->
    try_to_connect(),
    {next_state, connecting, Data#data{conn = undefined, event_stream = undefined}};
handle_event(info, {'EXIT', _HTTPPid, _Msg}, connected, Data) ->
    {keep_state, Data#data{event_stream = undefined}}.

handle_connect_response({error, Err}, connecting, Data = #data{host = Host, port = Port}) ->
    ?LOG_INFO(#{
        what => grpc_connection_failed,
        why => Err,
        host => Host,
        port => Port
    }),
    {ok, _TRef} = timer:apply_after(5000, gen_statem, cast, [self(), connect]),
    {keep_state, Data};
handle_connect_response({ok, Conn}, connecting, Data = #data{}) ->
    gen_statem:cast(self(), connect_stream),
    {next_state, connected, Data#data{conn = Conn, event_stream = undefined}}.

try_to_connect() ->
    gen_statem:cast(self(), connect).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State, _Data) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Data, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
