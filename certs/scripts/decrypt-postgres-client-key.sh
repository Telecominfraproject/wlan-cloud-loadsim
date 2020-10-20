#!/bin/sh
echo Generating decrypted version of the client key
openssl rsa -passin pass:mypassword -in postgresclientkey.pem -out postgresclientkey_dec.pem

