-module(features_store_lib_configmap).
-include_lib("kernel/include/logger.hrl").
-behaviour(features_store).
-export([init/0,
         get_all/1,
         store/2]).

-record(state, {api=defined,
                configmap_ref=undefined,
                additional_namespaces=undefined}).

%%%%
%   features_store api
%%%%

init() ->
    {ok, Namespace} = application:get_env(features, namespace),
    ConfigmapRef = #{namespace=>Namespace,
                     name=><<"features-state-store">>},
    Operations = [
        <<"createCoreV1NamespacedConfigMap">>,
        <<"readCoreV1NamespacedConfigMap">>,
        <<"replaceCoreV1NamespacedConfigMap">>
    ],
    API = kuberlnetes:load([{operations, Operations}]),
    Ops = swaggerl:operations(API),
    {ok, Namespaces} = application:get_env(features, namespaces),
    AdditionalNamespaces = [{NS, <<"getkimball-features">>} ||
                                 NS <- Namespaces],
    ?LOG_DEBUG(#{what=><<"Additional namespaces">>,
                 namespaces=>Namespaces,
                 tuples=>AdditionalNamespaces}),

    ?LOG_DEBUG(#{what=><<"Kubernetes operations">>,
                 ops=>Ops}),
    #state{api=API,
           configmap_ref=ConfigmapRef,
           additional_namespaces=AdditionalNamespaces}.

get_all(State=#state{configmap_ref=#{namespace:=NS, name:=Name}}) ->
    {Code, ConfigMapResp} = get_configmap(State, NS, Name),
    Data = case Code of
      404 -> ConfigMap = configmap(Name, []),
             create_configmap(State, NS, Name, ConfigMap),
             [];
      200 -> data_from_configmap_doc(ConfigMapResp)
    end,

    {Data, State}.

store(Data, State=#state{configmap_ref=#{namespace:=NS, name:=Name},
                         additional_namespaces=ANS}) ->
    ToWrite = [{NS, Name} | ANS],
    ok = write_configmap_to_namespaces(State, ToWrite, Data),
    {ok, State}.

write_configmap_to_namespaces(_State, [], _Data) ->
    ok;
write_configmap_to_namespaces(State, [{NS, Name}|T], Data) ->
    ok = write_configmap(State, NS, Name, Data),
    write_configmap_to_namespaces(State, T, Data).

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

create_configmap(#state{api=API}, NS, Name, Doc) ->
    Fields = configmap_request_fields(NS, Name, Doc),
    Resp = swaggerl:op(API, "createCoreV1NamespacedConfigMap", Fields),
    ?LOG_INFO(#{what=><<"Create Configmap">>,
                 response=>Resp,
                 namespace=>NS,
                 configmap=>Doc,
                 name=>Name}),
    Code = maps:get(<<"code">>, Resp, 200),
    Code.

replace_configmap(#state{api=API}, NS, Name, Doc) ->
    Fields = configmap_request_fields(NS, Name, Doc),
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

configmap_request_fields(NS, Name, Doc) ->
    [
        {<<"namespace">>, NS},
        {<<"name">>, Name},
        {<<"body">>, Doc}
    ].
