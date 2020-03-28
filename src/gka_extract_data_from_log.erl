-module(gka_extract_data_from_log).
-include_lib("kernel/include/logger.hrl").

-export([app/1]).


app(_Log = #{<<"kubernetes">> :=
              #{<<"namespace_name">> := NamespaceName,
                <<"container_name">> := ContainerName}}) ->

    {[<<"namespace">>, <<"container">>],
     [NamespaceName, ContainerName]}.
