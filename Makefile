PROJECT = erlang_ddd_cargo_sample
PROJECT_DESCRIPTION = Erlang DDD Cargo Sample 
PROJECT_VERSION = 0.0.1
DEPS = uuid mnesia_utile

dep_mnesia_utile = git https://github.com/gregormey/mnesia_utile master

include erlang.mk
