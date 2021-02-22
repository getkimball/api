# Get Kimball Documentation

The Kimball application is designed to run in your Kubernetes cluster to provide a feature flag API and analytics for your applications

## Getting Started with the Application

* [Application installation](/install.md)
* Integrating into your applications
* Creating [feature flags](/feature-flags.md)
* [Analytics](/analytics.md)
* [Predictions](/predictions.md)

## Architecture

The Kimball application contains two main components, the Core API, and Relays.

![Architecture Diagram](/img/architecture-overview.png)

### Core API

The Core API contains the synchronization, storage, and UI serving logic. When deployed in Kubernetes this run with a [Deployment](https://kubernetes.io/docs/concepts/workloads/controllers/deployment/).

API calls are synchronous and blocking until operations complete.

#### HTTP API

The HTTP API is used for setting/retrieving features and analytic data. It is defined with an [OpenAPI v3](https://swagger.io/) specification available via your locally installed instance at `/api-docs/index.html`

#### Storage

The Core API is responsible for managing storage of feature flag and analytic data.

Feature flags are stored as a [ConfigMap](https://kubernetes.io/docs/concepts/configuration/configmap/) in the cluster. The ConfigMap is loaded by Relays to provide per-Node caching of feature flag data.

Analytic data uses a pluggable storage configuration. Currently supported options:

* [Amazon S3](https://aws.amazon.com/s3/) and compatible APIs
* [Google Cloud Storage](https://cloud.google.com/storage/)
* Local file storage

#### User Interface

A basic UI is included for managing feature flags and viewing analytics. It is available via HTTP at the root path `/`.

### Relays

The Relays contain a cache of feature flag configuration and local API intended to reduce latency to the Core API.  When deployed in Kubernetes this is run as a [DaemonSet](https://kubernetes.io/docs/concepts/workloads/controllers/daemonset/) to provide a per-node deployed API/cache.

Relays are not required and are most helpful when using feature flags or when sending analytic events directly from your applications. They can be disabled in the [Helm chart](https://github.com/getkimball/charts/API) when deploying to Kubernetes.

API Calls are synchronous but will complete tasks asynchronously where possible.

* Retrieving features flags will be synchronously retrieved from the local cache
* Storing analytic data will be forwarded from the Relay to the Core API asynchronously
