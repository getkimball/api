## Feature Flag Model

Kimball Feature flags have a control specification but are presented to applications as boolean values.

Feature flags default to `false` and are made `true` by specifying conditions for when that will happen.

### Feature Flag Specification Types

* [Boolean](#boolean-flags) - A simple `true` or `false type.
* [Rollout](#rollout-flags) - An increasing chance of `true` over a period of time
* [User Conditions](#user-flags) - A user object sent when requesting features is evaluated on various conditions


#### Boolean Flags

Boolean flags are the simplest way to set a `true`/`false` condition for a feature flag. Setting this to `true` will mean the flag is always true, regardless of the other settings.

#### Rollout Flags

Rollout flags have a start and end time. The start time defaults to the current time if not specified.

Starting at 0%, the chance a rollout will evaluate to `true` increases linearly over the rollout period.

After the rollout end time the flag will always evaluate to `true`.

#### User Flags

User flags rely on an input object that is matched to the specification. This can be used to enable features for particular users or groups of users.


### Feature Flag Evaluation

The conditions for feature flags are evaluated from most-specific to least-specific. This leads to an evaluation order of

* User conditions
* Rollout
* Binary

The evaluation "wants" a flag to be `true`. If any condition is met a `true` will be returned.
