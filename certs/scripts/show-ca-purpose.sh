#!/bin/sh
openssl x509 -purpose -in ./testCA/cacert.pem -inform PEM -noout

