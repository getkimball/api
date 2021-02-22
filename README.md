# Kimball API

[![codecov](https://codecov.io/gh/getkimball/api/branch/trunk/graph/badge.svg?token=gVDJrLnoUY)](https://codecov.io/gh/getkimball/api)

The Kimball API is a self-hosted analytics service that takes events from your application and generates predictions about user behaviors.

[Talk to us](https://calendly.com/get-kimball/) about how we can help with your analytics/prediction problems!

See our [documentation](/docs/) about how to install and use the Kimball API. The remainder of this file contains information related to developing the Kimball API itself.

# Running


## Use [nix](https://nixos.org/manual/nix/stable/#chap-installation)!

The `shell.nix` file contains the required development tools. Use `nix-shell` to get started

## Running app

```
make deps app shell
```

```
make live-js
```

## Running locally with Docker

```
export TAG=kimball
docker build -t $TAG .
docker run -v ${PWD}/config/example.config:/kimball/app.config $(PWD)/config/example.config:/kimball/app.config -it -e LOG_LEVEL=debug $TAG
```

## Configuration

### Config file

An Erlang configuration file will be loaded from `/kimball/app.config` as part of the Release/Dockerfile. A user configuration file can be placed there to provide more complex configuration described below.

#### Deployment information

Information can be included about the deployment to help in future diagnosis of
problems. Right now this is the "site" and "cluster" information.

```
[{features, [
    {site, "Site name"},
    {cluster, "Cluster name"}
]}].
```

#### Counter Configuration

The initial bloom filter type, size, and error probability can be configured for counters. This is configured as a list with a regular expression matching the name of the filter.

* `pattern` - The regular expression
* `type` - `bloom_fixed_size` or `bloom_scalable`
* `date_cohort` - `weekly` or omitted. Whether to automatically generate counters for each week to track events over time.
* `size` - The fixed size or initial size (for `bloom_scalable` filters)
* `error_probability` - Bloom filter error probability

Example

```
[{features, [
    {counters, #{
        init => [
            #{pattern => ".*",
              type => bloom_fixed_size,
              date_cohort => weekly,
              size => 10000,
              error_probability => 0.01}
        ]
    }}
]}].
```

#### *Experimental* Event forwarding via GRPC Streams

The application can forward event streams via GRPC to external services. This is currently experimental, not well tested, and may incur significant performance concerns.

Proto file available at `src/proto/features_proto.proto`.

Configure such as

```
[{features, [
    {external_grpc_event_targets, [{"127.0.0.1", 8079}]}
]}].
```

#### *Experimental* Prediction request forwarding via GRPC RCP

API requests for the prediction API can also ask an external service for predictions and include them in the Kimball API response.

Proto file available at `src/proto/features_proto.proto`.

Configure such as

```
[{features, [
    {external_grpc_prediction_targets, [{"service name", "127.0.0.1", 8079}]}
]}].
```

### Environment variables

* `ADDITIONAL_NAMESPACES` - A comma separated list of namespaces to sync feature config to. This should include any namespaces where you intend to run sidecars
* `ANALYTICS_HOST` - Where to forward analytic events to if this process isn't storing them directly. This is used by the sidecar mode to know where to forward to an api-mode process.
* `API_PORT` - (default `8080`) Port where the HTTP API will be available.
* `AWS_ACCESS_KEY_ID` - Credentials for interacting with AWS
* `AWS_SECRET_ACCESS_KEY` - Credentials for interacting with AWS
* `FEATURES_MODE` - Which mode to start the application in
    * `api` (default) - Fully feature API server, storing state in configmaps
    * `sidecar` - Read only API meant to be deployed as a sidecar. Features features from `/features/data` volume in Kubernetes.
* `KUBERNETES_MEMORY_LIMIT` - The container limit in bytes, used for computing the metric `memory_remaining_bytes`
* `NAMESPACE` - Namespace to use for reading/writing in Kubernetes
* `S3_BUCKET` - AWS S3 Bucket to use for storage
* `S3_HOST` - AWS S3 Host to use for storage. This will attempt to auto configure when running in AWS.
* `GCS_BUCKET` - Google Cloud Storage Bucket to use for storage
* `GOOGLE_APPLICATION_CREDENTIALS` - Path to a JSON Service Account Key
* `STORAGE_PATH_PREFIX` - Path prefix to use when storing files in S3/GCS. Defaults to the installation namespace.

## Metrics

System metrics are available at `/metrics`

Some important ones:

* `kimball_counters` - The number of counters registered with the router. Equivalent to the number of events tracked.
* `kimball_persist_counters_managed` - The number of counters the persistence manager triggered in the last run. Should track, but lag, `kimball_counters`.

Metrics for each counter are available at `/metrics/counters`

* `kimball_counter` - Event counters

* `kimball_counter_weekly` - Per week counters if `date_cohort => weekly` is set.

Metrics for goal/event predictions are available at `/metrics/predictions`

* `kimball_bayes_prediction` - Prediction that users who complete `event` label will completed the `goal` label

## Releasing

```
git checkout trunk
git pull
TAG=$(date +"%Y.%m.%d")
git tag ${TAG}
git push origin ${TAG}
```

## Updating JS components

* Find outdated dependencies with `npm outdated` and update in `package.json
* Run `npm update`

... I think

# License

Apache 2.0. Copyright [Get Kimball Inc.](https://getkimball.com) 2020
