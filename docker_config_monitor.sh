#!/bin/sh

# This file is used from within the docker build script to copy the proper configurations for the manager
cp priv/templates/simmonitor.config.template.docker config/sys.config
cp priv/templates/simmonitor.args.template.docker config/vm.args

