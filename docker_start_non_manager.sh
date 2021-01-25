#!/bin/sh

# This file in invoked from within docker to start the application. It should not be used otherwise.
cd /owls
erl -config config/sys.config -args_file config/vm.args -name $ERL_NODE_NAME -noinput -noshell -detached -pa deps/*/ebin -pa ebin