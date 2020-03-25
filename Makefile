PROJECT = hellerl_world
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy
BUILD_DEPS = elvis_mk
LOCAL_DEPS = sasl
TEST_DIR = tests

dep_cowboy = git https://github.com/ninenines/cowboy.git 2.7.0
dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0

DEP_PLUGINS = elvis_mk

SHELL_OPTS = -eval 'application:ensure_all_started(hellerl_world).' -config sys

include erlang.mk
