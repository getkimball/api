# Kimball Analytics

Analytics with the Kimball application are designed to use a predictable amount
of CPU/memory, without risking details about individual users.

This data is used to compute [predictions](/predictions.md).

*note: See your local installation `/api-docs/index.html` for your particular API specification as it may differ from what is documented here.*

*POST requests must have the content-type of `application/json`*

## What can you track

Analytic events tracked within the application are designed to avoid storing personal information and instead track counts of specific events while still being able to track user uniqueness.

### Events

Analytic events are a name combined with a count of the number of unique users and are the basis for all analytics data.

### Goals

Goals are events that also count which events a user has completed before this goal.

### Value

An event can be submitted with a number "value" that will be tracked. This is exposed as a sum (and with the counter of the event to get an average).

## Namespaces

Namespaces provide logical separation for events, goals, and predictions. A default namespace of `default` is used unless another namespace is provided.

Incoming events can include a `namespace` parameter in the event body and requests for data can include a `?namespace=...` query argument.

## Definitions

ID/User ID - The "high cardinality" ID to track for an event.
Event Name - The action completed by the user
Goal - A predetermined event that you wish to track that will collect events these users have also completed.

*note: User IDs are tracked only on the first occurrence. A user that performs an event twice will only show up once in the analytics.*

## Submitting Analytic Events

### While requesting feature flags

This can be done by including a `user_id` and `feature` in the `user_obj` and `context_obj` fields of a feature flag request. [Example](https://github.com/getkimball/example-python-application/blob/76d0cce51ecf14bb4c36bc928471390be464f5f9/example.py#L22-L28).

### Sending individual analytic events

Individual analytic events can be sent to `/v0/analytics` with `event_name` and `user_id` properties in the POST request body.

```
{
  "event_name": "string",
  "user_id": "string"
}
```

### Sending multiple analytic events

Multiple analytic events can be sent to `/v0/analytics` with `event_name` and `user_id` properties in the POST request body.

```
{
  "events": [
    {"event_name": "string", "user_id": "string"},
    {"event_name": "string", "user_id": "string"}
}
```

Events can have multiple unique `event_name` and `user_id`s in the same request.

## Goals

Goals events are created with the API that will collect other events from that user when the user completes a goal event. This allows [funnel analysis](https://en.wikipedia.org/wiki/Funnel_analysis) of your application by tracking the user events leading up the goal.

The analytics for a goal event will include additional information beyond what a non-goal event includes:

* `event_counts` - This is a collection of the unique combinations of user events seen before the user completed the goal.

If a user `U` completes events `A`, `B`, `C`, and then goal `G`, `G` will have the event counts:

`[A, B, C]` - 1

This is mean to answer questions such as "how many features do users interact with before buying"

* `single_event_counts` - This is a collection of each of the events users have completed before the goal.

If a user `U` completes events `A`, `B`, `C`, and then goal `G`, `G` will have the single event counts:

`A` - 1
`B` - 1
`C` - 1

This is meant to answer questions such as "how many people who signed up have see a particular landing page".


### Create a goal

POST a `goal_name` to `/v0/goals` to create the goal

```
{
  "goal_name": "string"
}
```

### Creating a goal when submitting an event

Events can be submitted with a flag to ensure an event is marked as a goal. This removes the need to create goals before submitting infrequent events. The `ensure_goal` option is used for this. This will work when submitting single or multiple events. A `false` value will not disable the goal status for an event.

Example event:

```
{
  "event_name": "string",
  "user_id": "string",
  "ensure_goal": true
}
```

### Including event value when submitting an event

Events can be submitted with a `value` field that will be tracked as a total value for an event. The value will only be added to the total if the user is completing this event for the first time. Subsequent event occurrences by a single user will not affect stored values even if the first event did not have a value.

These totalled values will be exposed as a `value.sum` when retrieving analytic events. An average can be computed client side when combined with the count of an event.

### Retrieving Analytic Data

Analytic data can be retrieved via GET `/v0/analytics` which will return a list of analytic events seen, and details about each event.

* `name` - The event that was seen
* `count` -  The number of unique user IDs for this event
* `event_counts` - Data will only be included if this is a goal event. A list JSON objects with properties:
    * `events` - A list of events a user completed before triggering this goal
    * `count` - The number of unique user IDs with this combination of events
* `single_event_counts` - Data will only be included if this is a goal event. A list JSON objects with properties:
    * `name` - The name of the event a user has completed
    * `count` - The number of unique user IDs with this event
* `value` - Aggregate data about the `value` submitted with events
    * `sum` - The summation of `value`s submitted, counted only on the first event of each user. Subsequent occurrences of the event for the user will not be included.

### Event Namespaces

The namespaces known to the system can be retrieved with a GET request to `/v0/namespaces`

```
{"namespaces":[
    "default",
    "example"
]}
```

#### Prometheus

Prometheus metrics are exported at `/metrics/counters`.

* `kimball_counter` - The count of unique users for an event
* `kimball_counter_weekly` - The count of unique users in a week for an event. Exists for events where `date_cohort` is enabled

### Example

A online store tracking feature usage of their store leading up to the first sale by a user might have the events

* `Sign up`
* `Login`
* `Searched for a product`
* `Browsed for a product`
* `Direct link to product`

With the goal of

* `Completed purchase`

When a purchase is completed the events a user has completed will be stored with the goal. As multiple users complete purchases, the goal event data may include combinations such as:

* [ `Sign up`, `Login`, `Searched for a product`] - X occurrences
* [ `Sign up`, `Login`, `Browsed for a product` ] - Y occurrences
* [ `Direct link to a product` ] - Z occurrences

## Advanced Configuration

Advanced configuration should be done in consultation with a Kimball engineer as some configuration combinations may cause faults when running the application.

### Counter initialization

Advanced configuration can be used to control the initialization arguments for the internal counters. This can be used when higher or lower precision is needed from the counters to support goals such as:

* Large numbers of unique users per counter, with high precision
* Large numbers of unique events with predictable memory characteristics
* Small numbers of unique users in low memory situations

The configuration takes the form of

```
[{features, [
    {counters, #{
        init => [
            #{pattern => PATTERN,
              type => TYPE,
              date_cohort => DATE_COHORT_TYPE,
              size => SIZE,
              error_probability => ERROR_PROBABILITY}
        ]
    }}
]}].
```

The list `init` contains maps with the keys/options (all options are required):

* `pattern` - A regular expression matching the name of an event/counter. The items in `init` are evaluated sequentially and the first matching pattern will be the arguments for the counter

* `type` - The general type of counter to be used. Currently supported:
    * `bloom_scalable` - A scalable
    * `bloom_fixed_size` - A fixed size bloom filter, best used when the number of users is limited and a known number. Typically used when tens or hundreds of users are tracked and memory is a concern
* `date_cohort` - `weekly` or omitted. This will generate a counter automatically based on the current week number. This can be used to track events that occur multiple times for a user (visible if in different weeks) or events over time.
* `size` - The fixed or initial size for the bloom filter in number distinct users.
* ` error_probability` - A desired error probability that 2 users may not be distinguishable. This probability is used in conjunction with `size` and certain combinations may be invalid. This is typically expressed as `0.1`, `0.01`, `0.001` and so on.

An example counter initialization configuration:

```
[{features, [
    {counters, #{
        init => [
            #{pattern => ".*",
              type => bloom_scalable,
              size => 10000,
              error_probability => 0.01}
        ]
    }}
]}].
```
