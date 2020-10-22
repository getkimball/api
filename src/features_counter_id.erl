-module(features_counter_id).

-include_lib("kernel/include/logger.hrl").

-export([create/1,
         create/2,
         create/3,
         global_counter_id/0,
         name/1,
         pattern_matcher_name/1,
         pattern_matcher_type/1,
         to_file_path/1,
         to_full_name/1,
         to_prometheus_label_keys/1,
         to_prometheus_label_values/1,
         to_prometheus_name/1,
         type/1]).


-record(id, {name     :: binary() | '_',
             type     :: internal | named | weekly | '_',
             data     :: undefined | {integer(), integer()} | '_'
}).

create(Name) ->
    create(Name, named).

create(Name, internal) ->
    #id{name=Name, type=internal};
create(Name, named) ->
    #id{name=Name, type=named}.

create(Name, weekly, {Year, Month}) ->
    #id{name=Name, type=weekly, data={Year, Month}}.

global_counter_id() ->
    create(<<"global_counter">>, internal).

name(#id{name=Name}) ->
    Name.

pattern_matcher_name(Name) ->
    #id{name=Name, type='_', data='_'}.

pattern_matcher_type(Type) ->
    #id{name='_', type=Type, data='_'}.

type(#id{type=Type}) ->
    Type.

to_file_path(#id{name=Name, type=weekly, data={Year, Week}}) ->
    filename:join([Name, integer_to_list(Year), integer_to_list(Week)]);
to_file_path(#id{name=Name}) ->
    Name.

to_full_name(#id{name=Name, type=weekly, data={Year, Week}}) ->
    YearBin = list_to_binary(integer_to_list(Year)),
    WeekBin = list_to_binary(integer_to_list(Week)),
    FullName = << Name/binary,
                  <<" ">>/binary,
                  YearBin/binary,
                  WeekBin/binary >>,
    FullName;
to_full_name(#id{name=Name}) ->
    Name.

to_prometheus_label_keys(#id{type=weekly}) ->
    [name, year, week];
to_prometheus_label_keys(#id{}) ->
    [name].

to_prometheus_label_values(#id{name=Name, type=weekly, data={Year, Week}}) ->
    [Name, Year, Week];
to_prometheus_label_values(#id{name=Name}) ->
    [Name].

to_prometheus_name(#id{type=weekly}) ->
    kimball_counter_weekly;
to_prometheus_name(#id{}) ->
    kimball_counter.
