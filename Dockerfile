FROM node:10 as NODE_BUILDER
WORKDIR /app/src/
ADD . /app/src

FROM erlang:22 AS builder
WORKDIR /app/src
ADD . /app/src
RUN make deps app
RUN make rel
RUN mv /app/src/_rel/features_release/features_*.tar.gz /app.tar.gz


FROM debian:buster

RUN apt-get update && apt-get install -y openssl && apt-get clean

COPY --from=builder /app.tar.gz /app.tar.gz

WORKDIR /app

RUN tar -xzf /app.tar.gz

CMD ["/app/bin/features_release", "foreground"]
