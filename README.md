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
