-module(features_store_lib_s3).

-include_lib("kernel/include/logger.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-behaviour(features_store_lib).
-export([init/1,
         get_all/1,
         aws_config/1,
         store/2]).

-record(state, {bucket=undefined,
                aws_config=undefined,
                path=undefined}).

%%%%
%   features_store api
%%%%

init({Type, Name}) ->
    FullName = filename:join(Type, Name),
    init(FullName);
init(Name) ->
    {ok, Bucket} = application:get_env(features, s3_bucket),
    {ok, BasePath} = application:get_env(features, s3_base_path),
    {ok, AutoAWSConfig} = erlcloud_aws: auto_config(),

    AWSConfig = case application:get_env(features, s3_host) of
        {ok, ""} -> AutoAWSConfig;
        {ok, Host} -> AutoAWSConfig#aws_config{s3_host=Host}
    end,

    {ok, BasePath} = application:get_env(features, s3_base_path),
    Path = ensure_list(filename:join(BasePath, Name)),
    #state{bucket=Bucket,
           aws_config=AWSConfig,
           path=Path}.

ensure_list(I) when is_binary(I) ->
    binary:bin_to_list(I);
ensure_list(I) when is_list(I) ->
    I.

get_all(State=#state{bucket=Bucket, path=Path, aws_config=AWSConfig}) ->
    Obj = try erlcloud_s3:get_object(Bucket, Path, AWSConfig) of
        Returned -> Content = proplists:get_value(content, Returned),
                    erlang:binary_to_term(Content)
    catch
        error:{aws_error, {http_error, 404, _Msg, _Doc}} -> #{}
    end,
    Data = Obj,
    {Data, State}.

store(Data, State=#state{bucket=Bucket, path=Path, aws_config=AWSConfig}) ->
    Encoded = erlang:term_to_binary(Data),
    _Resp = erlcloud_s3:put_object(Bucket, Path, Encoded, AWSConfig),
    {ok, State}.


aws_config(#state{aws_config=AWSConfig}) ->
    AWSConfig.
