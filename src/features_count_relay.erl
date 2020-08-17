%%%-------------------------------------------------------------------
%%% @copyright 2020 Get Kimball Inc.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(features_count_relay).
-include_lib("kernel/include/logger.hrl").

-export([add/1,
         add/2,
         add/3]).

add(Items) when is_list(Items) ->
    ok.

add(EventName, Key)  ->
    add(EventName, Key, #{}).

add(EventName, Key, Opts) when is_binary(EventName), is_integer(Key) ->
    KeyB = list_to_binary(integer_to_list(Key)),
    add(EventName, KeyB, Opts);
add(EventName, Key, Opts) when is_binary(EventName), is_binary(Key) ->
    AnalyticsURL = persistent_term:get({features, analytics_url}),

    send_event(AnalyticsURL, EventName, Key, Opts).

send_event(undefined, EventName, Key, Opts) ->
    ?LOG_INFO(#{
        what => <<"ANALYTICS_HOST not set">>,
        feature_name => EventName,
        user_id => Key,
        opts => Opts
    }),
    ok;
send_event(URL, EventName, Key, Opts) ->
    Data = #{
      <<"event_name">> => EventName,
      <<"user_id">> => Key,
      <<"ensure_goal">> => maps:get(ensure_goal, Opts, false)
    },
    ReqBody = jsx:encode(Data),
    ?LOG_INFO(#{
        what => <<"count_relay forwarding">>,
        feature_name => EventName,
        user_id => Key,
        opts => Opts
    }),
    send(URL, ReqBody).

send(URL, ReqBody) ->
    ReqHeaders = [{<<"content-type">>, <<"application/json">>}],
    Method = post,
    RequestOpts = [{timeout, 1000}],

    ?LOG_DEBUG(#{what => <<"features_count_relay 1.1">>,
                 b=>ReqBody,
                 method=>Method,
                 url=>URL,
                 headers=>ReqHeaders,
                 opts=>RequestOpts}),
    {ok, Code, _RespHeaders, ClientRef} = hackney:request(Method,
                                                          URL,
                                                          ReqHeaders,
                                                          ReqBody,
                                                          RequestOpts),
    {ok, Body} = hackney:body(ClientRef),

    ?LOG_DEBUG(#{
      what => <<"features_count_relay request response">>,
      req_body => ReqBody,
      resp_body => Body,
      code => Code
    }),

    ok.
