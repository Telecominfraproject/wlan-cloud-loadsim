#!/bin/sh

erl -config config/sys.config -args_file config/vm.args -pa deps/*/ebin -pa ebin -tipauth 1
