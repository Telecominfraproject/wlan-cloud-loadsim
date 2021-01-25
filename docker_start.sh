#!/bin/sh

# This file in invoked from within docker to start the application. It should not be used otherwise.
cd /owls
erl -config config/sys.config -args_file config/vm.args -name $1 -pa deps/*/ebin -pa ebin