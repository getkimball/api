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
    try handle_req(Req, Spec, Handler, HandlerState) of
        {Req1, Code, Data, State} ->
            respond(Req1, Code, Data, State)
    catch
        {missing_required_key, Key} ->
            respond(Req,
                    400,
                    #{error => #{what=><<"Missing required element">>,
                                 key=>Key}},
                    []);
        {invalid_feature, Message} ->
            respond(Req,
                    400,
                    #{error => #{what=><<"Invalid feature">>,
                                 description=>ensure_binary(Message)}},
                    []);
        {invalid_date, Value} ->
            Msg = <<"Date doesn't appear to be the right format">>,
            respond(Req,
                    400,
                    #{error => #{what=>Msg,
                                 value=>ensure_binary(Value)}},
                    []);
        invalid_json ->
            Msg = <<"The request body is not valid JSON">>,
            respond(Req,
                    400,
                    #{error => #{what=>Msg}},
                    []);
        {incorrect_type, {Value, Type}} ->
            respond(Req,
                    400,
                    #{error => #{what=><<"Incorrect type">>,
                                 type_expected=>Type,
                                 value=>Value}},
                    [])
    end.

handle_req(Req, Spec, Handler, HandlerState) ->
    {Req1, Params} = params_from_request(Req, Spec),
    {HandlerReq, Code, Data, State} = Handler:handle_req(
                                                Req1,
                                                Params,
                                                HandlerState),
    _ResponseSpec = response_spec(Spec, Code),
    {HandlerReq, Code, Data, State}.

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
    Fun = fun(K, PropSpec, AccIn) ->
        % Data in from jsx will be binaries, not atoms
        KBin = erlang:atom_to_binary(K, utf8),
        DataValue = maps:get(KBin, Data, undefined),
        ValidDataValue = validate_property_spec(DataValue, PropSpec),
        maps:put(K, ValidDataValue, AccIn)
    end,

    Params = maps:fold(Fun, #{}, Properties),
    assert_has_keys(Required, Params),
    Params.

validate_property_spec(undefined, _Spec) ->
    undefined;
validate_property_spec(Value, _Spec=#{type := boolean}) ->
    case Value of
        true -> Value;
        false -> Value;
        _ -> throw({incorrect_type, {Value, boolean}})
    end;
validate_property_spec(Value, _Spec=#{type := string, format := 'date-time'}) ->
    case is_binary(Value) of
        false -> throw({incorrect_type, {Value, string}});
        _ -> StringValue = binary:bin_to_list(Value),
             try calendar:rfc3339_to_system_time(StringValue) of
                Date -> Date
             catch
                error:{badmatch, _DateValue} ->
                    throw({invalid_date, Value})
             end

    end;
validate_property_spec(Value, _Spec=#{type := string}) ->
    case is_binary(Value) of
        true -> Value;
        false -> throw({incorrect_type, {Value, string}})
    end.


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
                case jsx:is_json(Body) of
                    true -> Data = jsx:decode(Body, [return_maps]),
                            {CaseReq, Data};
                    false -> throw(invalid_json)
                end
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


ensure_binary(Bin) when is_binary(Bin) ->
    Bin;
ensure_binary(List) when is_list(List) ->
    binary:list_to_bin(List).
