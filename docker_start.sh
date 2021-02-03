#!/bin/sh

# This file in invoked from within docker to start the application. It should not be used otherwise.
cd /owls
case $ERL_NODE_TYPE in
  manager)
    cp priv/templates/simmanager.args.template.docker config/vm.args
    cp priv/templates/simmanager.config.template.docker config/sys.config
    erl -config config/sys.config -args_file config/vm.args -name $ERL_NODE_NAME $ERL_OPTIONS -pa deps/*/ebin -pa ebin -tipauth $TIP_AUTH
    ;;
  node)
    cp priv/templates/simnode.args.template.docker config/vm.args
    cp priv/templates/simnode.config.template.docker config/sys.config
    erl -config config/sys.config -args_file config/vm.args -name $ERL_NODE_NAME $ERL_OPTIONS -pa deps/*/ebin -pa ebin -tipauth $TIP_AUTH
    ;;
  monitor)
    cp priv/templates/simmonitor.args.template.docker config/vm.args
    cp priv/templates/simmonitor.config.template.docker config/sys.config
    erl -config config/sys.config -args_file config/vm.args -name $ERL_NODE_NAME $ERL_OPTIONS -pa deps/*/ebin -pa ebin -tipauth $TIP_AUTH
    ;;
esac

