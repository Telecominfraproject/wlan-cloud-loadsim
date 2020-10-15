PROJECT = mqttsim
PROJECT_DESCRIPTION = Erlang Based MQTT Traffic Simulator
PROJECT_VERSION = 0.1.0

DEPS = lager
ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk
