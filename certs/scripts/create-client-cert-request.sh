#!/bin/sh
openssl req -batch -config openssl-client.cnf -newkey rsa:2048 -sha256 -out clientcert.csr -outform PEM -nodes

