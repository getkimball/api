%%%-------------------------------------------------------------------
%%% @copyright 2020 Get Kimball Inc.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(features_count_relay).
-include_lib("kernel/include/logger.hrl").

-export([add/2]).

add(FeatureName, Key) when is_binary(FeatureName), is_integer(Key) ->
    KeyB = list_to_binary(integer_to_list(Key)),
    add(FeatureName, KeyB);
add(FeatureName, Key) when is_binary(FeatureName), is_binary(Key) ->
    AnalyticsURL = persistent_term:get({features, analytics_url}),
    send(AnalyticsURL, FeatureName, Key).

send(undefined, FeatureName, Key) ->
    ?LOG_INFO(#{
        what => <<"ANALYTICS_HOST not set">>,
        feature_name => FeatureName,
        user_id => Key
    }),
    ok;
send(URL, FeatureName, Key) ->
    Data = #{
      <<"feature_name">> => FeatureName,
      <<"user_id">> => Key
    },
    ReqBody = jsx:encode(Data),
    ReqHeaders = [{<<"content-type">>, <<"application/json">>}],
    Method = post,
    Opts = [{timeout, 1000}],

    ?LOG_DEBUG(#{what => <<"features_count_relay 1.1">>,
                 b=>ReqBody,
                 method=>Method,
                 url=>URL,
                 headers=>ReqHeaders,
                 opts=>Opts}),
    {ok, Code, _RespHeaders, ClientRef} = hackney:request(Method,
                                                          URL,
                                                          ReqHeaders,
                                                          ReqBody,
                                                          Opts),
    {ok, Body} = hackney:body(ClientRef),

    ?LOG_DEBUG(#{
      what => <<"features_count_relay request response">>,
      req_body => ReqBody,
      resp_body => Body,
      code => Code
    }),

    ok.
