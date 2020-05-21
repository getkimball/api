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
    {HandlerReq, Code, Data, Opts} = Handler:handle_req(Req, HandlerState),
    _ResponseSpec = response_spec(Spec, Code),
    respond(HandlerReq, Code, Data, Opts).


respond(Req, Code, Value, Opts) ->
    Resp = cowboy_req:reply(Code, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(Value), Req),
    {ok, Resp, Opts}.

%%%%
%   Internal
%%%%

method_metadata(Handler, Method) ->
    LowerMethod = string:lowercase(Method),
    [Trails] = Handler:trails(),
    Metadata = trails:metadata(Trails),
    MethodSpec = maps:get(LowerMethod, Metadata),
    MethodSpec.

response_spec(Spec, Code) ->
    Responses = maps:get(responses, Spec, #{}),
    CodeSpec = maps:get(Code, Responses),
    CodeSpec.

response_spec(Handler, Method, Code) ->
    Spec = method_metadata(Handler, Method),
    response_spec(Spec, Code).
