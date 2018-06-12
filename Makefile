PROJECT = ecucumber

PROJECT_DESCRIPTION = An open source port of Cucumber for Erlang

DEPS = egherkin

dep_egherkin = git https://github.com/jabberbees/egherkin

include erlang.mk

include debug.mk
