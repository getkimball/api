# Examples

Examples of using the kimball application. These assume you have a locally running application at port `8080`, the default when running the server in development mode from this project.

### Python Requirements

The [load.py](/examples/load.py) requires [Python requirements listed in /examples/requirements.txt](/examples/requirements.txt).

## Batch Loading Philadelphia 311 data

The [City of Philadelphia 311 dataset](https://www.opendataphilly.org/dataset/311-service-and-information-requests) is downloaded as a CSV, split apart by column, and loaded with a unique id for the service request ID.

The service request status field is loaded as a goal.

Data is loaded with an example [load.py](/examples/load.py) script that will batch upload rows of data.

[See the example code](/examples/philly-311.sh)


### Predictions

Once data is loaded predictions can be made regarding the properties loaded.

#### Predicting ticket status based on agency


```
% curl 'http://localhost:8080/v0/predictions?event=agency:Revenue' | python -mjson.tool
{
    "goals": {
        "status:Closed": {
            "likelihood": 0.16829971181556194,
            "no": 0.0,
            "yes": 0.016829971181556196
        },
        "status:Open": {
            "likelihood": 0.0,
            "no": 0.3510382513661202,
            "yes": 0.0
        },
        "status:status": {
            "likelihood": 0.0,
            "no": 1606.0,
            "yes": 0.0
        }
    }
}
```

With an open `likelihood` of 0.35 vs close `likelihood` of 0.16 there is a better chance of tickets never being closed by the Revenue department.

```
curl 'http://localhost:8080/v0/predictions?event=agency:Streets%20Department' | python -mjson.tool
{
    "goals": {
        "status:Closed": {
            "likelihood": 2.1160073356038773,
            "no": 0.004485197799318837,
            "yes": 0.21160073356038775
        },
        "status:Open": {
            "likelihood": 0.02119651347068146,
            "no": 4.413551912568306,
            "yes": 0.09355191256830601
        }
    }
}
```

The Streets Department has a better chance of closing tickets with an open `likelihood` of 0.02 and a closed `likelihood` of 2.1.

#### Predicting ticket status based on Zip code

```
curl 'http://localhost:8080/v0/predictions?event=zip:19143' | python -mjson.tool
{
    "goals": {
        "status:Closed": {
            "likelihood": 0.011841760544930573,
            "no": 0.00040869793031176314,
            "yes": 0.0011841760544930573
        },
        "status:Open": {
            "likelihood": 0.08524590163934427,
            "no": 0.024699453551912567,
            "yes": 0.008524590163934427
        }
    }
}
```
