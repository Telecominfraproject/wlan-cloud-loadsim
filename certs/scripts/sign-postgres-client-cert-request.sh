#!/bin/sh
openssl ca -batch -key mypassword -config openssl-ca.cnf -policy signing_policy -extensions signing_req_client -out postgresclientcert.pem -infiles postgresclientcert.csr

rm postgresclientcert.csr

