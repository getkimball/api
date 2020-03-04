PROJECT = hellerl_world
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

BUILD_DEPS = elvis_mk

dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0

DEP_PLUGINS = elvis_mk

include erlang.mk
