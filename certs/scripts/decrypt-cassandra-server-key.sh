#!/bin/sh
echo Generating decrypted version of the cassandra client/server key
openssl rsa -passin pass:mypassword -in cassandraserverkey.pem -out cassandraserverkey_dec.pem

