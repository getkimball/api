# Running

## Use nix!

## Running app

```
make deps app shell
```

## Running fluent-bit dummy

```
docker run --net=host -v$(pwd):/fluent-bit/etc -it fluent/fluent-bit:1.4.1
```
