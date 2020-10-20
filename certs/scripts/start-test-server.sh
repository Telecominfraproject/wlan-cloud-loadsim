#!/bin/sh
openssl s_server -CAfile ./testCA/cacert.pem -cert servercert.pem -key serverkey.pem -port 4242
