-module(features_store_lib_configmap).
-include_lib("kernel/include/logger.hrl").
-behaviour(features_store).
-export([init/0,
         get_all/1,
         store/2]).

-record(state, {api=defined,
                configmap_ref=undefined}).

init() ->
    ConfigmapRef = #{namespace=><<"getkimball">>,
                     name=><<"features-state-store">>},
    Operations = [
        <<"createCoreV1NamespacedConfigMap">>,
        <<"readCoreV1NamespacedConfigMap">>,
        <<"replaceCoreV1NamespacedConfigMap">>
    ],
    API = kuberlnetes:load([{operations, Operations}]),
    Ops = swaggerl:operations(API),
    ?LOG_DEBUG(#{what=><<"Kubernetes operations">>,
                 ops=>Ops}),
    #state{api=API, configmap_ref=ConfigmapRef}.

get_all(State=#state{api=API, configmap_ref=#{namespace:=NS, name:=Name}}) ->
    Fields = [
        {<<"name">>, Name},
        {<<"namespace">>, NS}
    ],
    ConfigMapResp = swaggerl:op(API, "readCoreV1NamespacedConfigMap", Fields),
    ?LOG_DEBUG(#{what=><<"Get Configmap">>,
                 namespace=>NS,
                 value=>ConfigMapResp,
                 name=>Name}),

    Code = maps:get(<<"code">>, ConfigMapResp, 200),
    Data = case Code of
      404 -> create_configmap(State),
             [];
      200 -> data_from_configmap_doc(ConfigMapResp)
    end,

    {Data, State}.


data_from_configmap_doc(#{<<"data">> := #{<<"data">> := Data}}) ->
    jsx:decode(Data, [return_maps]).

create_configmap(#state{api=API, configmap_ref=#{namespace:=NS, name:=Name}}) ->
    ConfigMap = configmap(Name, []),
    Fields = [
        {<<"namespace">>, NS},
        {<<"body">>, ConfigMap}
    ],
    Resp = swaggerl:op(API, "createCoreV1NamespacedConfigMap", Fields),
    ?LOG_DEBUG(#{what=><<"Create Configmap">>,
                 response=>Resp,
                 namespace=>NS,
                 configmap=>ConfigMap,
                 name=>Name}),


    ok.

store(Data, State=#state{api=API,
                         configmap_ref=#{namespace:=NS,
                                         name:=Name}}) ->
    ConfigMap = configmap(Name, Data),
    Fields = [
        {<<"name">>, Name},
        {<<"namespace">>, NS},
        {<<"body">>, ConfigMap}
    ],
    Resp = swaggerl:op(API, "replaceCoreV1NamespacedConfigMap", Fields),

    ?LOG_DEBUG(#{what=><<"Write Configmap">>,
                 response=>Resp,
                 namespace=>NS,
                 data=>Data,
                 name=>Name}),

    Code = maps:get(<<"code">>, Resp, 200),
    ok = case Code of
      200 -> ok
    end,
    State.


configmap(Name, Data) ->
    Serialized = jsx:encode(Data),
    #{<<"apiVersion">> => <<"v1">>,
      <<"kind">> => <<"ConfigMap">>,
      <<"metadata">> => #{
          <<"name">> => Name
      },
      <<"data">> => #{
          <<"data">> => Serialized
      }
    }.
