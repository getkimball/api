-module(features_store_lib_file).

-include_lib("kernel/include/logger.hrl").

-behaviour(features_store_lib).

-export([
    init/1,
    get_all/1,
    store/2
]).

-record(state, {path = undefined}).

%%%%
%   features_store api
%%%%

init("features_store") ->
    {ok, Path} = application:get_env(features, file_store_path),
    #state{path = Path};
init(Name) ->
    {ok, RootPath} = application:get_env(features, file_store_path),
    Path = filename:join([RootPath, Name]),
    #state{path = Path}.

get_all(State = #state{path = Path}) ->
    ReadFile = file:read_file(Path),
    B64Bin = handle_file(ReadFile, Path),
    DataBin = base64:decode(B64Bin),
    Data = erlang:binary_to_term(DataBin),

    {Data, State}.

handle_file({ok, DataBin}, _Path) ->
    DataBin;
handle_file({error, enoent}, Path) ->
    throw({enoent, Path}).

store(_Data, State = #state{}) ->
    {not_supported, State}.
