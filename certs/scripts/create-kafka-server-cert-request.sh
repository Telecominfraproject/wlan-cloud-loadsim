#!/bin/sh
openssl req -batch -config openssl-kafka-server.cnf -newkey rsa:2048 -sha256 -out kafkaservercert.csr -outform PEM
