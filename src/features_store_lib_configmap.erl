-module(features_store_lib_configmap).
-include_lib("kernel/include/logger.hrl").
-behaviour(features_store).
-export([init/0,
         get_all/1,
         store/2]).

-record(state, {api=defined,
                configmap_ref=undefined}).

%%%%
%   features_store api
%%%%

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

get_all(State=#state{configmap_ref=#{namespace:=NS, name:=Name}}) ->
    {Code, ConfigMapResp} = get_configmap(State, NS, Name),
    Data = case Code of
      404 -> create_configmap(State, NS, Name, []),
             [];
      200 -> data_from_configmap_doc(ConfigMapResp)
    end,

    {Data, State}.

store(Data, State=#state{configmap_ref=#{namespace:=NS, name:=Name}}) ->
    ok = write_configmap(State, NS, Name, Data),
    State.

%%%%
%   Internal
%%%%

get_configmap(#state{api=API}, NS, Name) ->
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
    {Code, ConfigMapResp}.



data_from_configmap_doc(#{<<"data">> := #{<<"data">> := Data}}) ->
    jsx:decode(Data, [return_maps]).

create_configmap(#state{api=API}, NS, Name, Data) ->
    ConfigMap = configmap(Name, Data),
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
    Code = maps:get(<<"code">>, Resp, 200),
    Code.


replace_configmap(#state{api=API}, NS, Name, Doc) ->
    Fields = [
        {<<"namespace">>, NS},
        {<<"body">>, Doc}
    ],
    Resp = swaggerl:op(API, "replaceCoreV1NamespacedConfigMap", Fields),
    ?LOG_DEBUG(#{what=><<"Replace Configmap">>,
                 response=>Resp,
                 namespace=>NS,
                 configmap=>Doc,
                 name=>Name}),
    Code = maps:get(<<"code">>, Resp, 200),
    Code.


write_configmap(State=#state{}, NS, Name, Data) ->
    ConfigMapDoc = configmap(Name, Data),

    ?LOG_DEBUG(#{what=><<"Write Configmap">>,
                 namespace=>NS,
                 configmap=>ConfigMapDoc,
                 name=>Name}),
    Code = replace_configmap(State, NS, Name, ConfigMapDoc),

    200 = case Code of
      200 -> 200;
      404 -> create_configmap(State, NS, Name, ConfigMapDoc)
    end,
    ok.


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
