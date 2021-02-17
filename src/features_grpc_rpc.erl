-module(features_grpc_rpc).

-include_lib("kernel/include/logger.hrl").

-export([predict/2]).

predict(Namespace, Events) ->
    Targets = application:get_env(features, external_grpc_prediction_targets, []),
    Req = #{namespace => Namespace, event_names => Events},

    ?LOG_INFO(#{
        what => grpc_prediction_req,
        request => Req
    }),
    TargetPrediction = fun({Name, Host, Port}) ->
        ?LOG_INFO(#{
            what => grpc_prediction_host_request,
            host => Host,
            port => Port
        }),
        {ok, Conn} = grpc_client:connect(tcp, Host, Port),
        Resp = grpc_client:unary(
            Conn,
            Req,
            'KimballIntegration',
            'Prediction',
            features_proto_pb,
            []
        ),
        ?LOG_INFO(#{
            what => grpc_prediction_response,
            response => Resp
        }),
        case Resp of
            {ok, #{result := #{ predictions := Predictions}}} -> build_prediction_structure(Name, Predictions);
            _ -> io:format("No match~n"), []
        end
    end,

    lists:flatten(lists:map(TargetPrediction, Targets)).


build_prediction_structure(Name, Predictions) ->
    Map = fun(#{prediction_name := PredictionName,
                yes := YesVal,
                no := NoVal}) ->
        NameBin = ensure_binary(Name),
        YesKey = << NameBin/binary, <<"_yes">>/binary >>,
        NoKey = << NameBin/binary, <<"_no">>/binary >>,
        #{<<"prediction_name">> => ensure_binary(PredictionName),
          YesKey => YesVal,
          NoKey => NoVal}
    end,
    lists:map(Map, Predictions).


ensure_binary(Bin) when is_binary(Bin) ->
    Bin;
ensure_binary(List) when is_list(List) ->
    binary:list_to_bin(List).
