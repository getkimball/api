-module(haproxy).
-include_lib("kernel/include/logger.hrl").

-export([frontends/0,
         request/2]).

-spec request(map(), binary()) -> any().
request(_Connection, _Request) ->
    ok.

frontends() ->
    URL = "http://localhost:5555/v1/services/haproxy/configuration/frontends",
    User = <<"dataplaneapi">>,
    Pass = <<"insecure-password">>,

    {ok, _Code, _Headers, Ref} = request_with_retry(get, URL, [], [],
                                                 [{basic_auth, {User, Pass}}],
                                                 3),
    {ok, Body} = hackney:body(Ref),
    Data = jsx:decode(Body, [return_maps]),
    Data.


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
