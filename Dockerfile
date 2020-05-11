FROM node:10 as NODE_BUILDER
WORKDIR /app/src/
ADD . /app/src

FROM erlang:22 AS builder
WORKDIR /app/src
ADD . /app/src
RUN make deps app
RUN make rel


FROM debian:buster

RUN apt-get update && apt-get install -y openssl && apt-get clean

COPY --from=builder /app/src/_rel/features_release/features_release-1.tar.gz /app.tar

WORKDIR /app

RUN tar -xzf /app.tar

CMD ["/app/bin/features_release", "foreground"]
