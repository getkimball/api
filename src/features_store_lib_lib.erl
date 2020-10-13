-module(features_store_lib_lib).

-export([name_to_path/2]).

name_to_path(Base, {Type, Name}) when is_binary(Name) ->
    filename:join([Base, Type, Name]);
name_to_path(Base, {Type, CounterID}) ->
    CounterPath = features_counter_id:to_file_path(CounterID),
    filename:join([
        Base,
        Type,
        CounterPath]);
name_to_path(Base, Name) when is_binary(Name) ->
    filename:join(Base, Name);
name_to_path(Base, CounterID) ->
    CounterPath = features_counter_id:to_file_path(CounterID),
    filename:join([Base, CounterPath]).
