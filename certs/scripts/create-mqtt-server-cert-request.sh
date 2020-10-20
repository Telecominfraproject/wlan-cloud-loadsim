#!/bin/sh
openssl req -batch -config mqtt-server.cnf -newkey rsa:2048 -sha256 -out mqttservercert.csr -outform PEM
