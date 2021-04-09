-module(cowboy_test_helpers).

-include_lib("eunit/include/eunit.hrl").

-export([
    http_get/4,
    http_get/5,
    http_post/3,
    http_post/4,
    http_post/5,
    read_reply/1,
    req/0,
    req/2,
    req/3,
    init/3,
    json_roundtrip/1,
    validate_response_against_spec/3,
    setup/0,
    cleanup/0
]).

setup() ->
    meck:new(cowboy_req, [passthrough]),
    meck:expect(
        cowboy_req,
        read_body,
        fun(Req = #{'_test_body' := Body}) -> {ok, Body, Req} end
    ),
    ok.

cleanup() ->
    ?assert(meck:validate(cowboy_req)),
    meck:unload(cowboy_req),
    ok.

init(Module, Req, Opts) ->
    InitResp = Module:init(Req, Opts),
    handle_init_response(Module, InitResp).

json_roundtrip(Obj) ->
    jsx:decode(jsx:encode(Obj), [return_maps]).

handle_init_response(_Module, {ok, Resp, State}) ->
    {ok, Resp, State};
handle_init_response(Module, {UpgradeMod, Req, State}) ->
    UpgradeMod:upgrade(Req, #{}, Module, State).

req() ->
    req(<<"GET">>, #{}).

req(post, raw, Opts) ->
    req(<<"POST">>, Opts);
req(post, json, Body) ->
    Data = jsx:encode(Body),
    req(<<"POST">>, #{'_test_body' => Data, has_body => true});
req(post, binary, Data) ->
    req(<<"POST">>, #{'_test_body' => Data, has_body => true}).

req(get, QueryArgs) ->
    QS = uri_string:compose_query(QueryArgs),
    req(<<"GET">>, #{qs => QS});
req(Method, Opts) when is_binary(Method) and erlang:is_map(Opts) ->
    Ref = make_ref(),
    Req = #{
        pid => self(),
        has_body => false,
        method => Method,
        headers => #{<<"content-type">> => <<"application/json">>},
        qs => <<"">>,
        streamid => Ref
    },
    MergedReq = maps:merge(Req, Opts),
    MergedReq.

http_get(Mod, Req, ExpectedCode, ExpectedData) ->
    http_get(Mod, #{}, Req, ExpectedCode, ExpectedData).

http_get(Mod, State, Req, ExpectedCode, ExpectedData) ->
    CowGetResp = cowboy_test_helpers:init(Mod, Req, State),
    {response, GetCode, GetHeaders, GetBody} = cowboy_test_helpers:read_reply(CowGetResp),

    Data = jsx:decode(GetBody, [return_maps]),
    ?assertEqual({ExpectedCode, ExpectedData}, {GetCode, Data}),

    GetSpec = specified_handler:response_spec(Mod, <<"get">>, ExpectedCode),
    ok = cowboy_test_helpers:validate_response_against_spec(GetSpec, GetHeaders, Data),
    ok.

http_post(Mod, Req, 204) ->
    http_post(Mod, Req, 204, no_body).

http_post(Mod, Req, ExpectedCode, ExpectedBody) when is_integer(ExpectedCode) ->
    http_post(Mod, [], Req, ExpectedCode, ExpectedBody);
http_post(Mod, State, Req, ExpectedCode) ->
    {PostCode, Data} = post(Mod, State, Req),
    ?assertEqual(ExpectedCode, PostCode),
    Data.

http_post(Mod, State, Req, ExpectedCode, ExpectedBody) ->
    {PostCode, Data} = post(Mod, State, Req),

    ?assertEqual({ExpectedCode, ExpectedBody}, {PostCode, Data}),
    ok.

post(Mod, State, Req) ->
    CowPostResp = cowboy_test_helpers:init(Mod, Req, State),
    {response, PostCode, _PostHeaders, PostBody} = cowboy_test_helpers:read_reply(CowPostResp),
    Data =
        case PostBody of
            <<"">> -> no_body;
            Content -> jsx:decode(Content, [return_maps])
        end,
    {PostCode, Data}.

read_reply({ok, #{streamid := StreamId}, _Opts}) ->
    receive
        {{_Pid, StreamId}, Msg} -> Msg
    after 10 -> error
    end.

validate_response_against_spec(
    _Spec = #{content := Content},
    _Headers = #{<<"content-type">> := ContentType},
    Data
) ->
    ContentTypeAtom = erlang:binary_to_atom(ContentType, utf8),
    #{ContentTypeAtom := #{schema := #{properties := Properties}}} = Content,
    SpecKeys = maps:keys(Properties),
    DataKeys = maps:keys(Data),
    ?assertEqual(SpecKeys, DataKeys),
    ok.
