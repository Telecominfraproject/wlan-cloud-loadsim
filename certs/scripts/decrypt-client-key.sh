#!/bin/sh
echo Generating decrypted version of the client key
openssl rsa -passin pass:mypassword -in clientkey.pem -out clientkey_dec.pem

