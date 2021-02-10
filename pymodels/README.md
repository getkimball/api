# Example External Models in Python

This directory contains code for an example external integration written in Python.


## How to install requirements

`../shell.nix` can install everything for you, or use `requirements.txt`

## Generate Python code from protobuf files

`python -m grpc_tools.protoc -I src/proto --python_out pymodels --grpc_python_out pymodels src/proto/features_proto.proto`

## How to run


Include in your Kimball app configuration of:

```
[{features, [
    {external_grpc_event_targets, [{"127.0.0.1", 8079}]}
]}].
```

Then start this example

`python3 example.py`
