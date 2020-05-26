-module(swagger_specified_handler).
-include_lib("kernel/include/logger.hrl").
-export([upgrade/4]).

-export([method_metadata/2,
         response_spec/2,
         response_spec/3]).


-callback handle_req(cowboy_req:req(), any()) ->
                {cowboy_req:req(),
                 integer(),
                 any(),
                 cowboy_http:opts()}.

upgrade(Req=#{method := Method}, _Env, Handler, HandlerState) ->
    Spec = method_metadata(Handler, Method),
    try params_from_request(Req, Spec) of
        {Req1, Params} ->
            {HandlerReq, Code, Data, Opts} = Handler:handle_req(
                                                Req1,
                                                Params,
                                                HandlerState),
            _ResponseSpec = response_spec(Spec, Code),
            respond(HandlerReq, Code, Data, Opts)
    catch
        {missing_required_key, Key} ->
            respond(Req,
                    400,
                    #{error => #{what=>"Missing required element",
                                 key=>Key}},
                    [])
    end.


respond(Req, Code, Value, Opts) ->
    Resp = cowboy_req:reply(Code, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(Value), Req),
    {ok, Resp, Opts}.

%%%%
%   Internal
%%%%

assert_has_keys([], _Map) ->
    ok;
assert_has_keys([H|T], Map) ->
    case maps:get(H, Map, undefined) of
        undefined -> throw({missing_required_key, H});
        _ -> ok
    end,
    assert_has_keys(T, Map),
    ok.

match_params(_Params=[], _BodyData) ->
    [];
match_params(_Params=[_H=#{name:=Name,
                           in:=body,
                           schema:=Schema}|T],
             BodyData) ->
    Param = match_schema(Schema, BodyData),
    [{Name, Param} | match_params(T, BodyData)].

match_schema(Schema=#{properties:=Properties}, Data) ->
    Required = maps:get(required, Schema, []),
    Fun = fun(K, _PropSpec, AccIn) ->
        % Data in from jsx will be binaries, not atoms
        KBin = erlang:atom_to_binary(K, utf8),
        DataValue = maps:get(KBin, Data, undefined),
        maps:put(K, DataValue, AccIn)
    end,

    Params = maps:fold(Fun, #{}, Properties),
    assert_has_keys(Required, Params),
    Params.


method_metadata(Handler, Method) ->
    LowerMethod = string:lowercase(Method),
    [Trails] = Handler:trails(),
    Metadata = trails:metadata(Trails),
    MethodSpec = maps:get(LowerMethod, Metadata),
    MethodSpec.

params_from_request(Req=#{has_body:=HasBody},
                    _Spec=#{parameters := SpecParams}) ->
    {Req1, BodyData} = case HasBody of
        false -> {Req, #{}};
        true -> {ok, Body, CaseReq} = cowboy_req:read_body(Req),
                Data = jsx:decode(Body, [return_maps]),
                {CaseReq, Data}
    end,

    Params = match_params(SpecParams, BodyData),

    {Req1, Params};
params_from_request(Req, _Spec) ->
    % No parameters in the Spec
    {Req, []}.

response_spec(Spec, Code) ->
    Responses = maps:get(responses, Spec, #{}),
    CodeSpec = maps:get(Code, Responses),
    CodeSpec.

response_spec(Handler, Method, Code) ->
    Spec = method_metadata(Handler, Method),
    response_spec(Spec, Code).
