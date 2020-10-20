#!/bin/sh

openssl req -batch -config postgres-client.cnf -newkey rsa:2048 -sha256 -out postgresclientcert.csr -outform PEM -nodes

