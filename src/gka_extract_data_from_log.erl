-module(gka_extract_data_from_log).
-include_lib("kernel/include/logger.hrl").

-export([app/1,
         label_values/2]).


app(_Log = #{<<"kubernetes">> :=
              #{<<"namespace_name">> := NamespaceName,
                <<"container_name">> := ContainerName},
             <<"log_processed">> :=
              #{<<"user_action">> := UserAction}}) ->

    {[<<"namespace">>, <<"container">>, <<"user_action">>],
     [NamespaceName, ContainerName, UserAction]}.


label_values([], _Log) ->
    [];
label_values([<<"container_name">>|T],
              Log = #{<<"kubernetes">> :=
                        #{<<"container_name">> := CN}}) ->

    [CN|label_values(T, Log)];
label_values([<<"namespace">>|T],
              Log = #{<<"kubernetes">> :=
                        #{<<"namespace_name">> := NS}}) ->

    [NS|label_values(T, Log)];
label_values([<<"user_action">>|T],
              Log = #{<<"log_processed">> :=
                        #{<<"user_action">> := UA}}) ->

    [UA|label_values(T, Log)].
