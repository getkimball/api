-module(cowboy_test_helpers).
-include_lib("eunit/include/eunit.hrl").
-export([read_reply/1,
         req/0,
         req/2,
         req/3,
         init/3,
         validate_response_against_spec/3,
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

init(Module, Req, Opts) ->
    InitResp = Module:init(Req, Opts),
    handle_init_response(Module, InitResp).

handle_init_response(_Module, {ok, Resp, State}) ->
    {ok, Resp, State};
handle_init_response(Module, {UpgradeMod, Req, State}) ->
    UpgradeMod:upgrade(Req, #{}, Module, State).

req() ->
    req(<<"GET">>, #{}).

req(post, json, Body) ->
    Data = jsx:encode(Body),
    req(<<"POST">>, #{'_test_body' => Data, has_body=>true});
req(post, binary, Data) ->
    req(<<"POST">>, #{'_test_body' => Data, has_body=>true}).

req(get, QueryArgs) ->
    QS = uri_string:compose_query(QueryArgs),
    req(<<"GET">>, #{qs=>QS});
req(Method, Opts) when is_binary(Method) and erlang:is_map(Opts) ->
    Ref = make_ref(),
    Req = #{pid => self(),
      has_body => false,
      method => Method,
      qs => <<"">>,
      streamid => Ref},
    MergedReq = maps:merge(Req, Opts),
    MergedReq.


read_reply({ok, #{streamid:=StreamId}, _Opts}) ->
    receive
        {{_Pid, StreamId}, Msg} -> Msg
    after 10 ->
        error
    end.

validate_response_against_spec(_Spec=#{content:=Content},
                               _Headers=#{<<"content-type">>:=ContentType},
                               Data) ->
    ContentTypeAtom = erlang:binary_to_atom(ContentType, utf8),
    #{ContentTypeAtom := #{schema := #{properties:=Properties}}} = Content,
    SpecKeys = maps:keys(Properties),
    DataKeys = maps:keys(Data),
    ?assertEqual(SpecKeys, DataKeys),
    ok.
