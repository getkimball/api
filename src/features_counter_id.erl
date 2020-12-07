-module(features_counter_id).

-include_lib("kernel/include/logger.hrl").

-export([
    create/1,
    create/3,
    create/4,
    global_counter_id/1,
    name/1,
    namespace/1,
    pattern_matcher_name/2,
    pattern_matcher_type/2,
    to_file_path/1,
    to_full_name/1,
    to_prometheus_label_keys/1,
    to_prometheus_label_values/1,
    to_prometheus_name/1,
    type/1
]).

-record(id, {
    namespace :: binary() | '_',
    name :: binary() | '_',
    type :: internal | named | weekly | '_',
    data :: undefined | {integer(), integer()} | '_'
}).

create(Name) ->
    create(<<"default">>, Name, named).

create(Namespace, Name, internal) ->
    #id{namespace = Namespace, name = Name, type = internal};
create(Namespace, Name, named) ->
    #id{namespace = Namespace, name = Name, type = named}.

create(Namespace, Name, weekly, {Year, Month}) ->
    #id{namespace = Namespace, name = Name, type = weekly, data = {Year, Month}}.

global_counter_id(Namespace) ->
    create(Namespace, <<"global_counter">>, internal).

name(#id{name = Name}) ->
    Name.

namespace(#id{namespace = Namespace}) ->
    Namespace.

pattern_matcher_name(Namespace, Name) ->
    #id{namespace = Namespace, name = Name, type = '_', data = '_'}.

pattern_matcher_type(Namespace, Type) ->
    #id{namespace = Namespace, name = '_', type = Type, data = '_'}.

type(#id{type = Type}) ->
    Type.

to_file_path(#id{namespace = Namespace, name = Name, type = weekly, data = {Year, Week}}) ->
    filename:join([Namespace, Name, integer_to_list(Year), integer_to_list(Week)]);
to_file_path(#id{namespace = Namespace, name = Name}) ->
    filename:join([Namespace, Name]).

to_full_name(#id{name = Name, type = weekly, data = {Year, Week}}) ->
    YearBin = list_to_binary(integer_to_list(Year)),
    WeekBin = list_to_binary(integer_to_list(Week)),
    FullName = <<Name/binary, <<" ">>/binary, YearBin/binary, WeekBin/binary>>,
    FullName;
to_full_name(#id{name = Name}) ->
    Name.

to_prometheus_label_keys(#id{type = weekly}) ->
    [kimball_namespace, name, year, week];
to_prometheus_label_keys(#id{}) ->
    [kimball_namespace, name].

to_prometheus_label_values(#id{
    namespace = Namespace,
    name = Name,
    type = weekly,
    data = {Year, Week}
}) ->
    [Namespace, Name, Year, Week];
to_prometheus_label_values(#id{namespace = Namespace, name = Name}) ->
    [Namespace, Name].

to_prometheus_name(#id{type = weekly}) ->
    kimball_counter_weekly;
to_prometheus_name(#id{}) ->
    kimball_counter.
