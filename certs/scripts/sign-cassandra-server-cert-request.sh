#!/bin/sh
openssl ca -batch -key mypassword -config openssl-ca.cnf -policy signing_policy -extensions signing_req_server -out cassandraservercert.pem -infiles cassandraservercert.csr

