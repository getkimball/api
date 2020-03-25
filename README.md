# Running

## Use nix!

## Running app

```
make deps app shell
```

## Running fluent-bit dummy

```
fluent-bit -i dummy -t dummy -o http://localhost:8080/incoming -m '*' -p format=json
```
