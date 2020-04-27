PROJECT = service_controller
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy jsx prometheus prometheus_cowboy hackney swaggerl kuberlnetes
BUILD_DEPS = elvis_mk
LOCAL_DEPS = sasl
TEST_DEPS = meck
TEST_DIR = tests

dep_cowboy = git https://github.com/ninenines/cowboy.git 2.7.0
dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0
dep_hackney = hex 1.15.2
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.10.0
dep_kuberlnetes = git https://github.com/philipcristiano/kuberlnetes.git v0.0.3
dep_prometheus = git https://github.com/deadtrickster/prometheus.erl.git v4.5.0
dep_prometheus_cowboy = hex 0.1.8
dep_swaggerl = git https://github.com/philipcristiano/swaggerl.git v0.0.5

DEP_PLUGINS = elvis_mk

SHELL_OPTS = -eval 'application:ensure_all_started(service_controller).' -config sys

include erlang.mk
