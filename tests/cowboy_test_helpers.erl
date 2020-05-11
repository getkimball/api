-module(cowboy_test_helpers).
-include_lib("eunit/include/eunit.hrl").
-export([read_reply/1,
         req/0,
         req/3,
         setup/0,
         cleanup/0]).

setup() ->
    meck:new(cowboy_req, [passthrough]),
    meck:expect(cowboy_req, read_body,
        fun(Req=#{'_test_body' := Body}) -> {ok, Body, Req} end),
    ok.

cleanup() ->
    ?assert(meck:validate(cowboy_req)),
    meck:unload(cowboy_req),
    ok.

req() ->
    req(<<"GET">>, #{}).

req(post, json, Body) ->
    Data = jsx:encode(Body),
    req(<<"POST">>, #{'_test_body' => Data, has_body=>true}).

req(Method, Opts) when is_binary(Method) and erlang:is_map(Opts) ->
    Ref = make_ref(),
    Req = #{pid => self(),
      method => Method,
      streamid => Ref},
    MergedReq = maps:merge(Req, Opts),
    MergedReq.


read_reply({ok, #{streamid:=StreamId}, _Opts}) ->
    receive
        {{_Pid, StreamId}, Msg} -> Msg
    after 10 ->
        error
    end.
