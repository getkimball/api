-module(gka_extract_data_from_log).
-include_lib("kernel/include/logger.hrl").

-export([app/1]).


app(_Log = #{<<"kubernetes">> :=
              #{<<"namespace_name">> := NamespaceName,
                <<"container_name">> := ContainerName},
             <<"log_processed">> :=
              #{<<"user_action">> := UserAction}}) ->

    {[<<"namespace">>, <<"container">>, <<"user_action">>],
     [NamespaceName, ContainerName, UserAction]}.
