# Kimball Predictions

Predications in the Kimball App use the [analytics](/analytics.md) data to generate [Bayesian conditional probabilities](https://en.wikipedia.org/wiki/Bayesian_statistics).

Predictions are available at `/v0/predictions` in your local installation.

Probabilities are returned in the response for goals that have at least 1 user that has completed an event before the goal. Goals with 0 users or users that have completed no other events are not included in the response.

## Example output

For a hypothetical set of data consisting of

Events:
  * Example Event A
  * Example Event B

Goals:
  * Example Goal Event

the prediction API might return:

```
{
  "goals":{
    "Example Goal Event": {
      "events":{
          "Example Event A": {"bayes": 0.5},
          "Example Event B": {"bayes": 0.25}
      }
    }
  }
}
```

This response displays the information:

* There is a 50% probability that users who complete `Example Event A` will complete `Example Goal Event`
* There is a 25% probability that users who complete `Example Event B` will complete `Example Goal Event`


## Multi Event Predictions

The predictions endpoint accepts `?event=...` query string arguments (multiple instances can be provided as separate `event` arguments) to give predictions for goals based on users with those events.

This is computed via a [Naive Bayes algorithm](https://en.wikipedia.org/wiki/Naive_Bayes_classifier)

```
{
  "goals": {
    "Example Goal Event":{
      "no":0.0004,
      "yes":0.0205
    }
  }
}
```

## User Based Predictions

The predictions endpoint accepts `?user_id=...` query string argument to give predictions for goals based on the events known for that user.

* `user_id` and `event` parameters cannot be combined in a request (HTTP 400 response).
* A user without events will not generate predictions (HTTP 404 response).

This is computed via a [Naive Bayes algorithm](https://en.wikipedia.org/wiki/Naive_Bayes_classifier)

```
{
  "goals": {
    "Example Goal Event":{
      "no":0.0004,
      "yes":0.0205
    }
  }
}
```

#### Prometheus

Prometheus metrics are exported at `/metrics/predictions`.

* `kimball_bayes_prediction` - The prediction that users completing the (label) event will complete (label) goal. Range 0-1.
