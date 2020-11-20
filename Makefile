PROJECT = owls
PROJECT_DESCRIPTION = OpenWiFi Load Simulator (OWLS)
PROJECT_VERSION = 0.1.0

DEPS = lager gpb jiffy cowlib ranch gun cowboy rec2json uuid

ERLC_OPTS += -I deps/gpb/include
ERLC_OPTS += +'{parse_transform, lager_transform}'
ERLC_OPTS += +'debug_info'
ERLC_OPTS += -Ddebug

# Apps necessary for dialyzer
PLT_APPS += ssl os_mon sasl crypto inets eunit syntax_tools public_key mnesia tftp

dep_cowlib_commit = master
dep_cowboy_commit = master
dep_ranch_commit = master
dep_gun_commit = master

include erlang.mk

# Code necessary to generate the protobufs for mqtt payload
$(PROJECT).d:: src/opensync_stats.erl

src/opensync_stats.erl:: protobuf/opensync_stats.proto
	$(gen_verbose) deps/gpb/bin/protoc-erl protobuf/opensync_stats.proto -o-erl src -o-hrl include -Ideps/gpb/include
