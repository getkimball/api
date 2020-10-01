-module(features_store_lib_lib).


-export([name_to_path/2]).

name_to_path(Base, {Type, Name}) ->
    filename:join([Base, Type, Name]);
name_to_path(Base, Name) ->
    filename:join(Base, Name).
