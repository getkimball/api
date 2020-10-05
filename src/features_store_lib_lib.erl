-module(features_store_lib_lib).

-include("counter_names.hrl").

-export([name_to_path/2]).

name_to_path(Base, {Type,
                    #counter_name_weekly{name=Name,
                                         year=Year,
                                         week=Week}}) ->
    filename:join([
        Base,
        Type,
        Name,
        integer_to_list(Year),
        integer_to_list(Week)]);
name_to_path(Base, {Type, Name}) ->
    filename:join([Base, Type, Name]);
name_to_path(Base, #counter_name_weekly{name=Name,
                                         year=Year,
                                         week=Week}) ->
    filename:join([
        Base,
        Name,
        integer_to_list(Year),
        integer_to_list(Week)]);
name_to_path(Base, Name) ->
    filename:join(Base, Name).
