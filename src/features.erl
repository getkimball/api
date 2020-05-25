-module(features).

-export([collapse_to_boolean/1,
         collapse_features_map/1]).

collapse_to_boolean(#{boolean := Boolean}) ->
    Boolean.


%%%%
%   Internal
%%%%

collapse_features_map(Map) ->
    maps:fold(fun collapse_fun/3, #{}, Map).

collapse_fun(K, V, AccIn) ->
    NewV = collapse_to_boolean(V),
    maps:put(K, NewV, AccIn).
