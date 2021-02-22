# Use Case - Churn


Determine:

* What "init" event to use for discovering users
    * What properties of users to track - subscription level?
* How to determine they have churned
    * Are there cancellations?
    * Can you track how long it has been since someone has logged in/been seen
        * ~"last seen N weeks ago"

Need to do:

* Enable weekly cohorts for each goal
* Enable weekly cohorts for discovery event ( that way we can see longer term engagement )

## Tracking churn

An event for the user needs to be seen. For tracking churn this can be any event, just so that the system knows there is a user

### Identifying churn

#### By last usage

A "last usage" event allows for making the decision of "what is churn" after the data is collected. As multiple weeks accumulate it will be possible to see what users have in common as they reach this stage.

```
{
  "event_name": "last-login-4-weeks",
  "user_id": "[USER ID]",
  "ensure_goal": true
}
```

#### By cancellation

On cancellation send event

```
{
  "event_name": "cancelled",
  "user_id": "[USER ID]",
  "ensure_goal": true
}
```

## Predicting churn

### Track usage to predict churn

Collecting events about the type of usage of your application by user will allow prediction for churn


```
{
  "event_name": "[APPLICATION CONTROLLER / FEATURE]",
  "user_id": "[USER ID]",
}

