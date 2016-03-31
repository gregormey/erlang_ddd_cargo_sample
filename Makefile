# Project
PROJECT = erlang_ddd_cargo_sample
PROJECT_DESCRIPTION = Erlang DDD Cargo Sample 
PROJECT_VERSION = 0.0.1

# Options.
CT_OPTS += -pa test -ct_hooks erlang_ddd_cargo_sample_ct_hook []

# Depandancies
DEPS = uuid mnesia_utile
dep_mnesia_utile = git https://github.com/gregormey/mnesia_utile master

TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/extend/ct_helper.git master

include erlang.mk
