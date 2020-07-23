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

### Environment variables

* `ADDITIONAL_NAMESPACES` - A comma separated list of namespaces to sync feature config to. This should include any namespaces where you intend to run sidecars
* `ANALYTICS_HOST` - Where to forward analytic events to if this process isn't storing them directly. This is used by the sidecar mode to know where to forward to an api-mode process.
* `API_PORT` - (default `8080`) Port where the HTTP API will be available.
* `AWS_ACCESS_KEY_ID` - Credentials for interacting with AWS
* `AWS_SECRET_ACCESS_KEY` - Credentials for interacting with AWS
* `FEATURES_MODE` - Which mode to start the application in
    * `api` (default) - Fully feature API server, storing state in configmaps
    * `sidecar` - Read only API meant to be deployed as a sidecar. Features features from `/features/data` volume in Kubernetes.
* `NAMESPACE` - Namespace to use for reading/writing in Kubernetes

* `S3_BUCKET` - Bucket to use for storage


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
