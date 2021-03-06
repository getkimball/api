PROJECT = features
PROJECT_DESCRIPTION = New project

DEPS = \
	jiffy \
	cortex_remote_write \
	cowboy \
	cowboy_swagger \
	enenra \
	eraven \
	erlcloud \
	etbloom \
	grpc_client \
	hackney \
	jsx \
	kuberlnetes \
	prometheus \
	prometheus_cowboy \
	ranch \
	recon \
	snappy \
	specified_handler \
	swaggerl \
	trails

BUILD_DEPS = version.mk erlfmt # sync
LOCAL_DEPS = sasl
TEST_DEPS = meck jesse
TEST_DIR = tests
DIALYZER_DIRS = --src src tests

dep_cortex_remote_write = git https://github.com/getkimball/cortex_remote_write 0.1.7
dep_cowboy = git https://github.com/ninenines/cowboy.git 2.8.0
dep_cowboy_swagger = hex 2.2.0
dep_enenra = git https://github.com/nlfiedler/enenra.git 0.4.1
dep_erlfmt = git https://github.com/WhatsApp/erlfmt.git v0.8.0
dep_eraven = git https://github.com/getkimball/eraven.git 2020-05-20
dep_erlcloud = git https://github.com/erlcloud/erlcloud.git 3.3.3
dep_etbloom = git https://github.com/getkimball/etbloom.git 1.0.1
dep_hackney = hex 1.16.0
dep_grpc_client = git https://github.com/getkimball/grpc_client.git 20210208
dep_jesse = git https://github.com/for-GET/jesse.git 1.5.5

# Jiffy is used by other deps, forcing an update here to make R23 compatible.
# There seems to be an upstream issue with Jiffy on some systems so use our
# fork that has a patch for that https://github.com/davisp/jiffy/issues/203
dep_jiffy = git https://github.com/getkimball/jiffy.git 1.0.5-getkimball

dep_jsx = git https://github.com/talentdeficit/jsx.git v2.10.0
dep_kuberlnetes = git https://github.com/philipcristiano/kuberlnetes.git v0.0.5
dep_prometheus = git https://github.com/deadtrickster/prometheus.erl.git v4.6.0
dep_prometheus_cowboy = hex 0.1.8
dep_ranch = git https://github.com/ninenines/ranch.git 1.7.1
dep_recon = git https://github.com/ferd/recon.git 2.5.1
dep_snappy = git https://github.com/skunkwerks/snappy-erlang-nif.git master
dep_specified_handler = git https://github.com/getkimball/specified_handler.git 0.1.1
dep_swaggerl = git https://github.com/philipcristiano/swaggerl.git v0.0.7
dep_sync = git https://github.com/rustyio/sync.git master
dep_trails = hex 2.0.0
dep_version.mk = git https://github.com/manifest/version.mk.git v0.2.0

DEP_PLUGINS = version.mk

SHELL_OPTS = -eval 'application:ensure_all_started(features).' -config sys +S2

.DEFAULT_GOAL = all

.PHONY: live-js
live-js:
	npm run dev

erlfmt:
	$(gen_verbose) $(SHELL_ERL) -pa $(SHELL_PATHS) -eval 'erlfmt_cli:do("erlfmt", [write, {files, ["src/*.erl", "tests/*.erl"]} ]), halt(0)'

erlfmt_check:
	$(gen_verbose) $(SHELL_ERL) -pa $(SHELL_PATHS) -eval 'erlfmt_cli:do("erlfmt", [check, {files, ["src/*.erl", "tests/*.erl"]} ]), halt(0)'

.PHONY: proto # TODO, make this more intelligent
proto:
	deps/gpb/bin/protoc-erl -modsuffix "_pb" -maps -o-erl src src/proto/*

$(PROJECT).d:: proto

include erlang.mk
