-module(haproxy).
-include_lib("kernel/include/logger.hrl").

-export([frontends/0,
         backends/0,
         servers/1,
         binds/1,
         http_request_rules/2,
         ensure_frontend/2,
         ensure_backend/2,
         ensure_no_backend/1,
         ensure_bind/2,
         ensure_server/2]).

-define(JSON_HEADER, {<<"content-type">>, <<"application/json">>}).

base_url() ->
    "http://localhost:5555/v1/".

auth_option() ->
    User = <<"dataplaneapi">>,
    Pass = <<"insecure-password">>,
    {basic_auth, {User, Pass}}.

hap_request(Method, Path, Version, QueryArgs, Body, Attempts) ->
    URL = base_url() ++ Path,

    QueryArgsWithVersion = case Version of
        [] -> QueryArgs;
        Else -> VersionArg = [
                  {"version", list_to_binary(integer_to_list(Else))}
                ],
                QueryArgs ++ VersionArg
    end,

    URLWithParams = case QueryArgsWithVersion of
        [] -> URL;
        ElseParams -> URL ++ "?" ++ uri_string:compose_query(ElseParams)
    end,

    Payload = case Body of
        [] -> [];
        ElseBody -> jsx:encode(ElseBody)
    end,

    Headers = [?JSON_HEADER],

    request(Method, URLWithParams, Headers, Payload, Attempts).


request(Method, URL, Headers, Payload, Attempts) ->
    ?LOG_DEBUG(#{what=>"haproxy http request",
                 request_payload=>Payload,
                 request_method=>Method,
                 request_path=>URL}),

    {ok, Code, _Headers, Ref} = request_with_retry(
          Method, URL, Headers, Payload, Attempts),
    {ok, Body} = hackney:body(Ref),

    ?LOG_DEBUG(#{what=>"haproxy http response",
                 code=>Code,
                 request_method=>Method,
                 request_payload=>Payload,
                 request_path=>URL,
                 response=>Body}),
    Data = case Body of
        <<>> -> #{};
        Else -> jsx:decode(Else, [return_maps])
    end,

    ?LOG_DEBUG(#{what=>"haproxy http response",
                 code=>Code,
                 request_method=>Method,
                 request_payload=>Payload,
                 request_data=>Data,
                 request_path=>URL,
                 response=>Body}),

    http_code_transform(Method, Code, Data).

request_with_retry(_Method, _URL, _H, _P, 0) ->
    {error, no_more_retries};
request_with_retry(Method, URL, H, P, Retries) ->
    Opts = [auth_option()],
    case hackney:request(Method, URL, H, P, Opts) of
      {error, Reason} ->
          ?LOG_INFO(#{what=>"Request Error",
                       url=>URL,
                       payload=>P,
                       reason=>Reason,
                       retries_left=>Retries - 1}),
          request_with_retry(Method, URL, H, P, Retries -1);
      Else -> Else
    end.

ensure(PutPath, PostPath, VersionFunc, QueryArgs, Options) ->
    #{<<"_version">> := PutVersion} = VersionFunc(),
    Resp = hap_request(put, PutPath, PutVersion, QueryArgs, Options, 1),
    case Resp of
      ok -> ok;
      {error, not_found} ->
          #{<<"_version">> := PostVersion} = VersionFunc(),
          hap_request(post, PostPath, PostVersion, QueryArgs, Options, 1);
      Else -> Else
    end.


frontends() ->
    {ok, Body} = hap_request(
        get, "services/haproxy/configuration/frontends", [], [], [], 3),
    Body.

ensure_frontend(Name, #{}) when is_binary(Name) ->
    LName = binary:bin_to_list(Name),
    PutPath = "services/haproxy/configuration/frontends/" ++ LName,
    PostPath = "services/haproxy/configuration/frontends",
    FEOptions = frontend_options(Name, <<"">>),

    ensure(PutPath, PostPath, fun frontends/0, [], FEOptions).

frontend_options(Name, _BackendName) ->
    #{<<"name">> => Name,
     <<"mode">> => <<"http">>,
     % <<"default_backend">> => BackendName,
     <<"maxconn">> => 2000}.

backends() ->
    {ok, Body} = hap_request(
        get, "services/haproxy/configuration/backends", [], [], [], 3),
    ?LOG_INFO(#{what=><<"backends">>, body=>Body}),
    Body.

ensure_backend(Name, Options) when is_binary(Name) ->
    LName = binary:bin_to_list(Name),
    PutPath = "services/haproxy/configuration/backends/" ++ LName,
    PostPath = "services/haproxy/configuration/backends",
    BEOptions = Options,

    ensure(PutPath, PostPath, fun backends/0, [], BEOptions).

ensure_no_backend(Name) when is_binary(Name) ->
    LName = binary:bin_to_list(Name),
    Path = "services/haproxy/configuration/backends/" ++ LName,
    #{<<"_version">> := Version} = haproxy:backends(),
    hap_request(delete, Path, Version, [], [], 1).


binds(FEName) ->
    Path = "/services/haproxy/configuration/binds",
    QueryArgs = [{<<"frontend">>, FEName}],
    {ok, Body} = hap_request(get, Path, [], QueryArgs, [], 3),
    Body.

ensure_bind(Name, Opts) when is_binary(Name) ->
    LName = binary:bin_to_list(Name),
    PutPath = "services/haproxy/configuration/binds/" ++ LName,
    PostPath = "services/haproxy/configuration/binds",
    Options = #{
        name => Name,
        address => <<"0.0.0.0">>,
        port => maps:get(port, Opts)
    },
    QueryArgs = [{<<"frontend">>, Name}],

    ensure(PutPath, PostPath, fun() -> binds(Name) end, QueryArgs, Options).

servers(BEName) ->
    Path = "/services/haproxy/configuration/servers",
    QueryArgs = [{<<"backend">>, BEName}],
    {ok, Body} = hap_request(get, Path, [], QueryArgs, [], 3),
    Body.

ensure_server(Name, #{backend_name:=BEName,
                      cluster_ip:=ClusterIP,
                      port:=Port}) when is_binary(Name) ->
    LName = binary:bin_to_list(Name),
    PutPath = "services/haproxy/configuration/servers/" ++ LName,
    PostPath = "services/haproxy/configuration/servers",

    Options = #{
        name => BEName,
        address => ClusterIP,
        port => Port
    },
    QueryArgs = [{<<"backend">>, BEName}],

    ensure(PutPath, PostPath, fun() -> servers(BEName) end, QueryArgs, Options).

frontend_request_rules_query(Name) ->
    [{<<"parent_name">>, Name},
     {<<"parent_type">>, <<"frontend">>}].

http_request_rules(frontend, Name) ->
    Path = "/services/haproxy/configuration/http_request_rules",
    QueryArgs = frontend_request_rules_query(Name),
    hap_request(get, Path, [], QueryArgs, [], 3).

http_code_transform(get, Code, Body) ->
    case Code of
        404 -> {error, not_found};
        200 -> {ok, Body};
        202 -> {ok, Body};
        Else -> {error, {Else, Body}}
    end;
http_code_transform(put, Code, Body) ->
    case Code of
        404 -> {error, not_found};
        200 -> ok;
        202 -> ok;
        Else -> {error, {Else, Body}}
    end;
http_code_transform(delete, Code, Body) ->
    case Code of
        202 -> ok;
        204 -> ok;
        Else -> {error, {Else, Body}}
    end;
http_code_transform(post, Code, Body) ->
    case Code of
        202 -> ok;
        Else -> {error, {Else, Body}}
    end.
