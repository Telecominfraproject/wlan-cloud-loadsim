#!/bin/sh
openssl req -batch -config $$ROOT_DIR$$/ssl-client.cnf -newkey rsa:2048 -sha256 -out $$ROOT_DIR$$/clientcert.csr -outform PEM -nodes
openssl ca  -batch -config $$ROOT_DIR$$/ssl-ca.cnf -policy signing_policy -extensions signing_req_client -out $$ROOT_DIR$$/clientcert.pem -infiles $$ROOT_DIR$$/clientcert.csr
openssl rsa -in $$ROOT_DIR$$/clientkey.pem -out $$ROOT_DIR$$/clientkey_dec.pem
