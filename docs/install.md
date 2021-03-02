This repository provides installation resources for the Get Kimball applications

# Application Installation

Applications will be installed to your Kubernetes cluster.

Prerequisites:

* Target version of the application (latest `trunk-*` from [Docker Images](https://quay.io/repository/getkimball/api) is a good place to start.
* Sentry DSN if would like to capture errors to Sentry
* One or more Kubernetes clusters in which to install the applications
* [Helm](https://helm.sh/)
* An S3 Bucket writable from an AWS-instance metadata credential set (or other application storage credentials)

## Storage options

Feature flag data will be stored in Kubernetes ConfigMaps but analytics data will require external storage.

Options:

* S3
* Google Cloud Storage

### Google Cloud Storage

GCS storage requires a Service Account with read/write access to a GCS bucket

* Create a Kubernetes Secret in the namespace of the deployment, with the SA credentials for the value of the key `service_account.json`
* Pass in the secret name to Helm with `--set kimball.secret.name=$SECRET_NAME`
* Set the name of the bucket with `--set kimball.gcs_bucket=$GCS_BUCKET_NAME`

## Installation Process

Applications can then be updated to point to the Daemonset for the Kimball API

### Steps

* Docker login with your credentials

```
docker login --username [USERNAME] --password-stdin quay.io
```

* Download the image and push to your own repository

```
KIMBALL_VERSION=[PROVIDED BY KIMBALL]
SENTRY_DSN=[PROVIDED BY KIMBALL]

LOCAL_REPOSITORY=[YOUR DOCKER REPOSITORY]
KIMBALL_IMAGE=quay.io/getkimball/api:${KIMBALL_VERSION}
LOCAL_IMAGE=${LOCAL_REPOSITORY}:${KIMBALL_VERSION}

docker pull quay.io/getkimball/api:${KIMBALL_VERSION}
docker tag ${KIMBALL_IMAGE} ${LOCAL_IMAGE}
docker push ${LOCAL_IMAGE}
```

* Install Kimball API from the Helm chart

```
SERVICE_TYPE=ClusterIP

helm repo add getkimball https://getkimball.github.io/charts/stable
helm upgrade kimball-api getkimball/kimball-api --install \
  --namespace getkimball \
  --create-namespace \
  --wait \
  --timeout 5m \
  --set image.repository="${LOCAL_REPOSITORY}" \
  --set image.tag=${KIMBALL_VERSION} \
  --set service.type=${SERVICE_TYPE} \
  --set kimball.sentry_dsn=${SENTRY_DSN} \
  --set kimball.s3_bucket=${S3_BUCKET}
```

## Post installation

The Helm installation notes will contain information for how to reach your installation.

Applications should be configured to make requests to the node-local daemonset.


## Helpful configuration options for the Helm chart

* Omitting `kimball.s3_bucket` or other storage options will disable analytics persistence which is helpful in development/testing installs

* Omitting `kimball.sentry_dsn` will disable Sentry error logging. We recommend continuing to use Sentry so we can more easily assist resolving errors, but it may also send sensitive data to Sentry.


## Running on Heroku

The Docker images are deployable to Heroku. A starting install should configure the environment variables:

* `AWS_ACCESS_KEY_ID`
* `AWS_SECRET_ACCESS_KEY`
* `S3_BUCKET`

With a Docker image version and Heroku App name:

```
VERSION=...
QUAY_IMAGE=quay.io/getkimball/api:${VERSION}
HEROKU_APP=...


docker pull QUAY_IMAGE
docker tag QUAY_IMAGE registry.heroku.com/${HEROKU_APP}/web
heroku container:release web -a ${HEROKU_APP}
```

### Advanced configuration

More advanced configuration is available through the `kimball.app_config` option. The [Erlang Config format](http://erlang.org/doc/man/config.html) is used. Options through this configuration format are typically more complex than single flags would allow.

Example Helm option:

```
--set-file kimball.app_config=kimball.config
```

With a no-op config file having the form:

```
[{features, []
}].
```

#### Prometheus Remote Write

For more details about metrics see [monitoring](/docs/monitoring.md)

Prometheus metrics are exposed via the `/metrics` path of the application. Metrics can also be forwarded by the application to a remote Prometheus/Cortex. This may be useful if you are deploying this application in a remote location where you are unable to run Prometheus, but still want metrics.

This is provided by [Cortex Remote Write](https://github.com/getkimball/cortex_remote_write)

```
[{cortex_remote_write, [
    {interval, 15000},
    {url, "URL"},
    {username, "USERNAME"},
    {password, "PASSWORD"},
    {default_labels, [
      {"label_name", "label_value"}
    ]}
]}].
```

## Helpful commands

### Install our Docker Quay credentials into Kubernetes

(Not recommended)

```
kubectl create secret docker-registry getkimball-quay \
    --docker-server=quay.io \
    --docker-username=... \
    --docker-password=... \
    --docker-email=...
```

Then include

```
  --set imagePullSecrets=getkimball-quay
```
