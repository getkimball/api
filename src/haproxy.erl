-module(haproxy).
-include_lib("kernel/include/logger.hrl").

-export([frontends/0,
         ensure_frontend/2,
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

frontends() ->
    URL = base_url() ++ "services/haproxy/configuration/frontends",

    {ok, _Code, _Headers, Ref} = request_with_retry(get, URL, [], [],
                                                 [auth_option()],
                                                 3),
    {ok, Body} = hackney:body(Ref),
    Data = jsx:decode(Body, [return_maps]),
    Data.

ensure_frontend(Name, Options) when is_binary(Name) ->
    % LName = binary:bin_to_list(Name),
    #{<<"_version">> := PutVersion} = haproxy:frontends(),
    Resp = put_frontend(Name, PutVersion, Options),
    case Resp of
      ok -> ok;
      {error, not_found} ->
          #{<<"_version">> := PostVersion} = haproxy:frontends(),
          post_frontend(Name, PostVersion, Options);
      Else -> Else
    end.

put_frontend(Name, Version, Options) ->
    LName = binary:bin_to_list(Name),
    URL = base_url() ++ "services/haproxy/configuration/frontends/" ++ LName,
    Params = [{"version", list_to_binary(integer_to_list(Version))}],
    URLParams = uri_string:compose_query(Params),
    URLWithParams = URL ++ "?" ++ URLParams,

    Headers = [?JSON_HEADER],
    ROptions = maps:merge(#{<<"name">> => Name,
                            <<"mode">> => <<"http">>,
                            <<"maxconn">> => 2000}, Options),

    Payload = jsx:encode(ROptions),
    {ok, Code, _H, Body} = json_request(put, URLWithParams,
                                        Headers, Payload, [auth_option()]),
    ?LOG_DEBUG(#{what=>"ensure_frontend",
                 code=>Code,
                 request_headers=>Headers,
                 request_payload=>Payload,
                 request_url=>URLWithParams,
                 response=>Body}),
    case Code of
        404 -> {error, not_found};
        200 -> ok;
        Else -> {error, {Else, Body}}
    end.

post_frontend(Name, Version, Options) ->
    URL = base_url() ++ "services/haproxy/configuration/frontends",
    Params = [{"version", list_to_binary(integer_to_list(Version))}],
    URLParams = uri_string:compose_query(Params),
    URLWithParams = URL ++ "?" ++ URLParams,

    Headers = [?JSON_HEADER],
    ROptions = maps:merge(#{<<"name">> => Name,
                            <<"mode">> => <<"http">>,
                            <<"maxconn">> => 2000}, Options),

    Payload = jsx:encode(ROptions),
    {ok, Code, _H, Body} = json_request(post, URLWithParams,
                                        Headers, Payload, [auth_option()]),
    ?LOG_DEBUG(#{what=>"ensure_frontend",
                 code=>Code,
                 request_headers=>Headers,
                 request_payload=>Payload,
                 request_url=>URLWithParams,
                 response=>Body}),
    case Code of
        202 -> ok;
        Else -> {error, {Else, Body}}
    end.

json_request(Method, URL, ReqHeaders, Payload, Options) ->
    {ok, Code, RespHeaders, Ref} = hackney:request(Method,
                                               URL,
                                               ReqHeaders,
                                               Payload,
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
