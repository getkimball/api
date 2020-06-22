PROJECT = features
PROJECT_DESCRIPTION = New project

DEPS = ranch cowboy jsx prometheus prometheus_cowboy hackney trails cowboy_swagger swaggerl kuberlnetes eraven
BUILD_DEPS = elvis_mk version.mk covertool
LOCAL_DEPS = sasl
TEST_DEPS = meck jesse
TEST_DIR = tests
DIALYZER_DIRS = --src src tests

COVER = 1
NO_AUTOPATCH = covertool

dep_cowboy = git https://github.com/ninenines/cowboy.git 2.7.0
dep_trails = hex 2.0.0
dep_cowboy_swagger = hex 2.2.0
dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0
dep_eraven = git https://github.com/getkimball/eraven.git 2020-05-20
dep_hackney = hex 1.15.2
dep_jesse = git https://github.com/for-GET/jesse.git 1.5.5
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.10.0
dep_prometheus = git https://github.com/deadtrickster/prometheus.erl.git v4.5.0
dep_prometheus_cowboy = hex 0.1.8
dep_ranch = git https://github.com/ninenines/ranch.git 1.7.1
dep_kuberlnetes = git https://github.com/philipcristiano/kuberlnetes.git v0.0.5
dep_swaggerl = git https://github.com/philipcristiano/swaggerl.git v0.0.7
dep_version.mk = git https://github.com/manifest/version.mk.git v0.2.0

DEP_PLUGINS = elvis_mk version.mk

SHELL_OPTS = -eval 'application:ensure_all_started(features).' -config sys

include erlang.mk
