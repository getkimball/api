-module(haproxy).

-export([frontends/0,
         request/2]).

-spec request(map(), binary()) -> any().
request(_Connection, _Request) ->
    ok.

frontends() ->
    URL = "http://localhost:5555/v1/services/haproxy/configuration/frontends",
    User = <<"dataplaneapi">>,
    Pass = <<"insecure-password">>,

    {ok, _Code, _Headers, Ref} = hackney:request(get, URL, [], [],
                                                 [{basic_auth, {User, Pass}}]),
    {ok, Body} = hackney:body(Ref),
    Data = jsx:decode(Body, [return_maps]),
    Data.
