#!/bin/sh
echo Generating decrypted version of the mqtt server key
openssl rsa -passin pass:mypassword -in mqttserverkey.pem -out mqttserverkey_dec.pem

