PROJECT = betfair

PROJECT_DESCRIPTION = Betfair API
PROJECT_VERSION = 0.1.0

COVER = 1

DEPS = lager jsx gun gproc unistring
TEST_DEPS = meck

DIALYZER_DIRS = ebin

dep_unistring = git https://github.com/rambocoder/unistring.git

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

PLT_APPS = inets ssl crypto tools runtime_tools compiler eunit
