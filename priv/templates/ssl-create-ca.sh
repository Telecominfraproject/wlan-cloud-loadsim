#!/bin/sh

openssl genrsa -out $$ROOT_DIR$$/cakey.pem 4096
openssl req -config $$ROOT_DIR$$/ssl-ca.cnf -batch -new -x509 -days 3000 -sha256 -key $$ROOT_DIR$$/cakey.pem -out $$ROOT_DIR$$/cacert.pem -outform PEM

## openssl req -batch -x509 -days 3000 -config $$ROOT_DIR$$/ssl-ca.cnf -newkey rsa:4096 -out $$ROOT_DIR$$/cacert.pem -outform PEM