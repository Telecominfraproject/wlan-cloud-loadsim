#!/bin/sh
openssl req -batch -config $$ROOT_DIR$$/ssl-server.cnf -newkey rsa:2048 -sha256 -out $$ROOT_DIR$$/servercert.csr -outform PEM
openssl ca  -batch -config $$ROOT_DIR$$/ssl-ca.cnf -policy signing_policy -extensions signing_req_server -out $$ROOT_DIR$$/servercert.pem -infiles $$ROOT_DIR$$/servercert.csr
openssl rsa -in $$ROOT_DIR$$/serverkey.pem -out $$ROOT_DIR$$/serverkey_dec.pem
