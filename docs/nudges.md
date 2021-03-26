# Nudges

Nudges are the interventions the Kimball application recommends to prompt users for new behavior. Depending on your application these can be actions such as:

* Sending a reminder email
* Notifying a user in-app to explore a new feature

Nudges are per-user (or unique ID) for a particular goal. The recommended nudges will not include events the user has already completed.


## Example Nudge

For a request given a user ID and an example goal of `completed-sale` a response might look like:



```
curl http://localhost:8080/v0/nudges?user_id=&goal=completed-sale
{
  "nudges": [
    {"event_name": "added-item-to-cart"}
    {"event_name": "welcome-email-sent"},
  ]
  "user_id": "1",
  "goal": "compelted-sale"
}
```

This response would mean the `added-item-to-cart` has a high correlation with sales, while the welcome email has a lower correlation. Nudging the user to add something to their cart appears to be the best way to complete sales.
