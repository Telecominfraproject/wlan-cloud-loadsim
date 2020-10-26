PROJECT = mqttsim
PROJECT_DESCRIPTION = Erlang Based MQTT Traffic Simulator
PROJECT_VERSION = 0.1.0

DEPS = lager gpb jiffy ranch gun cowboy

ERLC_OPTS += -I deps/gpb/include
ERLC_OPTS += +'{parse_transform, lager_transform}'

dep_cowboy_commit = master
dep_ranch_commit = master

include erlang.mk

$(PROJECT).d:: src/opensync_stats.erl

src/opensync_stats.erl:: protobuf/opensync_stats.proto
	$(gen_verbose) deps/gpb/bin/protoc-erl protobuf/opensync_stats.proto -o-erl src -o-hrl include -Ideps/gpb/include
