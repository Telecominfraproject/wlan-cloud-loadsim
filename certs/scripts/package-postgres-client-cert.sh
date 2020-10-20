#!/bin/sh
openssl pkcs12 -export -in postgresclientcert.pem -inkey postgresclientkey.pem -passin pass:mypassword -passout pass:mypassword -out postgresclient.p12 -name user -CAfile testCA/cacert.pem -caname root -chain
