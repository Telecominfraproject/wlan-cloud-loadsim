#!/bin/sh
openssl req -batch -config openssl-server.cnf -newkey rsa:2048 -sha256 -out servercert.csr -outform PEM
