# Monitoring

Metrics are exposed in a format for Prometheus or compatible scraper to collect via the `/metrics` path.

Various system and application metrics are exposed via that endpoint, with helpful values documented below.

Additional metric exports are available at:

* [`/metrics/counters`](/analytics.md) - Metrics about the individual event counters
* [`/metrics/predictions`](/predictions.md) - Metrics about predictions for goals

These metrics are not included in the main metric exports as they can generate a significant number of metrics if there are many events / goals in the system.

## Helpful metrics

`kimball_counters` - The number of bloom filters/counters the application is tracking.

`kimball_event_add_duration_microseconds_(sum|count)` - Performance counters for the internal time spent on incrementing counters for goals and events.

`kimball_persist_counters_managed` - The number of bloom filters/counters persisted during the most recent persistence synchronization call

`memory_remaining_bytes` - Number of bytes the application thinks are free to use. This is calculated as the value of then environment variable `KUBERNETES_MEMORY_LIMIT` minus the total memory the Erlang thinks is used. This may not be representative of what the application process is using in a container. It is provided as a quick way to see if more memory should be added.
