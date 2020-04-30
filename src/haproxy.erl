-module(haproxy).
-include_lib("kernel/include/logger.hrl").

-export([frontends/0,
         backends/0,
         ensure_frontend/2,
         ensure_backend/2,
         request/2]).

-define(JSON_HEADER, {<<"content-type">>, <<"application/json">>}).

-spec request(map(), binary()) -> any().
request(_Connection, _Request) ->
    ok.

base_url() ->
    "http://localhost:5555/v1/".

auth_option() ->
    User = <<"dataplaneapi">>,
    Pass = <<"insecure-password">>,
    {basic_auth, {User, Pass}}.

retried_get(Path) ->
    URL = base_url() ++ Path,
    {ok, _Code, _Headers, Ref} = request_with_retry(get, URL, [], [],
                                                 [auth_option()],
                                                 3),
    {ok, Body} = hackney:body(Ref),
    Data = jsx:decode(Body, [return_maps]),
    Data.

hap_payload_request(Method, Path, Version, Opts) ->
    URL = base_url() ++ Path,
    Params = [{"version", list_to_binary(integer_to_list(Version))}],
    URLParams = uri_string:compose_query(Params),
    URLWithParams = URL ++ "?" ++ URLParams,
    Headers = [?JSON_HEADER],
    ?LOG_DEBUG(#{what=>"haproxy http request",
                 request_payload=>Opts,
                 request_method=>Method,
                 request_path=>URLWithParams}),
    {ok, Code, _Headers, Body} = json_request(Method, URLWithParams,
                                        Headers, Opts, [auth_option()]),
    ?LOG_DEBUG(#{what=>"haproxy http response",
                 code=>Code,
                 request_payload=>Opts,
                 request_path=>URLWithParams,
                 response=>Body}),
    {ok, Code, Body}.

frontends() ->
    retried_get("services/haproxy/configuration/frontends").

ensure_frontend(Name, _Options) when is_binary(Name) ->
    % LName = binary:bin_to_list(Name),
    #{<<"_version">> := PutVersion} = haproxy:frontends(),
    FEOptions = frontend_options(Name),
    Resp = put_frontend(Name, PutVersion, FEOptions),
    case Resp of
      ok -> ok;
      {error, not_found} ->
          #{<<"_version">> := PostVersion} = haproxy:frontends(),
          post_frontend(Name, PostVersion, FEOptions);
      Else -> Else
    end.


put_frontend(Name, Version, Options) ->
    LName = binary:bin_to_list(Name),
    Path = "services/haproxy/configuration/frontends/" ++ LName,
    {ok, Code, Body} = hap_payload_request(put, Path, Version, Options),

    case Code of
        404 -> {error, not_found};
        200 -> ok;
        Else -> {error, {Else, Body}}
    end.

post_frontend(_Name, Version, Options) ->
    Path = "services/haproxy/configuration/frontends",

    {ok, Code, Body} = hap_payload_request(post, Path, Version, Options),
    case Code of
        202 -> ok;
        Else -> {error, {Else, Body}}
    end.

frontend_options(Name) ->
    #{<<"name">> => Name,
     <<"mode">> => <<"http">>,
     <<"maxconn">> => 2000}.

backends() ->
    retried_get("services/haproxy/configuration/backends").

ensure_backend(Name, Options) when is_binary(Name) ->
    % LName = binary:bin_to_list(Name),
    #{<<"_version">> := PutVersion} = haproxy:backends(),
    Resp = put_backend(Name, PutVersion, Options),
    case Resp of
      ok -> ok;
      {error, not_found} ->
          #{<<"_version">> := PostVersion} = haproxy:backends(),
          post_backend(Name, PostVersion, Options);
      Else -> Else
    end.

put_backend(Name, Version, Options) ->
    LName = binary:bin_to_list(Name),
    Path = "services/haproxy/configuration/backends/" ++ LName,

    ROptions = maps:merge(#{<<"name">> => Name}, Options),

    {ok, Code, Body} = hap_payload_request(put, Path, Version, ROptions),
    case Code of
        404 -> {error, not_found};
        200 -> ok;
        202 -> ok;
        Else -> {error, {Else, Body}}
    end.

post_backend(Name, Version, Options) ->
    Path = "services/haproxy/configuration/backends/",

    ROptions = maps:merge(#{<<"name">> => Name}, Options),

    {ok, Code, Body} = hap_payload_request(post, Path, Version, ROptions),
    case Code of
        404 -> {error, not_found};
        202 -> ok;
        Else -> {error, {Else, Body}}
    end.

json_request(Method, URL, ReqHeaders, Payload, Options) ->
    JSONPayload = jsx:encode(Payload),
    {ok, Code, RespHeaders, Ref} = hackney:request(Method,
                                               URL,
                                               ReqHeaders,
                                               JSONPayload,
                                               Options),
    {ok, Body} = hackney:body(Ref),
    {ok, Code, RespHeaders, Body}.

request_with_retry(_Method, _URL, _H, _P, _Opts, 0) ->
    {error, no_more_retries};
request_with_retry(Method, URL, H, P, Opts, Retries) ->
    case hackney:request(Method, URL, H, P, Opts) of
      {error, Reason} ->
          ?LOG_INFO(#{what=>"Request Error",
                       url=>URL,
                       headers=>H,
                       payload=>P,
                       reason=>Reason,
                       retries_left=>Retries - 1}),
          request_with_retry(Method, URL, H, P, Opts, Retries -1);
      Else -> Else
    end.
