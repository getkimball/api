# Get Kimball Features API

[![codecov](https://codecov.io/gh/getkimball/features/branch/trunk/graph/badge.svg?token=gVDJrLnoUY)](https://codecov.io/gh/getkimball/features)

# Running

## As a client

Features is available through a Swagger-defined API.

A Swagger 2.0 spec is available at the path`/api-docs/swagger.json`.

When developing locally you should use the public UI: http://a9a25f850863945aeb2e4da40402e9a6-29786484.us-east-1.elb.amazonaws.com/api-docs

When deployed in cluster a `FEATURES_SWAGGER` environment variable will be available to your app that will point to the JSON.

In cluster these URLs are:
master-branch: http://master-features.getkimball.svc/api-docs/swagger.json
PR branch: http://features.getkimball.svc/api-docs/swagger.json

## Use nix!

## Running app

```
make deps app shell
```

```
make live-js
```

## Configuration

### Config file

An Erlang configuration file will be loaded from `/kimball/app.config` as part of the Release/Dockerfile. A user configuration file can be placed there to provide more complex configuration described below.

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

* `kimball_counters` - The number of counters registered with the router. Equivalent to the number of events tracked.
* `kimball_persist_counters_managed` - The number of counters the persistence manager triggered in the last run. Should track, but lag, `kimball_counters`.

## Releasing

```
git checkout master
git pull
TAG=$(date +"%Y.%m.%d")
git tag ${TAG}
git push origin ${TAG}
```

# License

All rights reserved. Copyright [Get Kimball Inc.](https://getkimball.com) 2020
