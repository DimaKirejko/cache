PROJECT = cache
PROJECT_DESCRIPTION = Caching http server
PROJECT_VERSION = 0.2.0

DEPS = cowboy jsx

LOCAL_DEPS = crypto ssl asn1

dep_cowboy_commit = 2.9.0

dep_jsx_commit = 2.9.0

DEP_PLUGINS = cowboy

RELX_DEPS = relx
RELX_CONFIG = $(CURDIR)/relx.config
RELX_OUTPUT_DIR ?= _rel

BUILD_DEPS += relx
include erlang.mk
