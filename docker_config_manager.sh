#!/bin/sh

# This file is used from within the docker build script to copy the proper configurations for the manager
cp priv/templates/simmanager.config.template.docker config/sys.config
cp priv/templates/simmanager.args.template.docker config/vm.args

