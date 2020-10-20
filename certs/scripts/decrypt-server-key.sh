#!/bin/sh
echo Generating decrypted version of the server key
openssl rsa -passin pass:mypassword -in serverkey.pem -out serverkey_dec.pem

