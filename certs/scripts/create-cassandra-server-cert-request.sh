#!/bin/sh
openssl req -batch -config openssl-cassandra-server.cnf -newkey rsa:2048 -sha256 -out cassandraservercert.csr -outform PEM
