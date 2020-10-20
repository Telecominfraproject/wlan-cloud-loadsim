#!/bin/sh
openssl pkcs12 -export -in cassandraservercert.pem -inkey cassandraserverkey.pem -passin pass:mypassword -passout pass:mypassword -out cassandra-server.pkcs12 -name 1 -CAfile testCA/cacert.pem -caname root -chain

keytool -importkeystore -destkeystore cassandra_server_keystore.jks -srckeystore cassandra-server.pkcs12 -srcstoretype pkcs12 -srcstorepass mypassword -deststorepass mypassword -deststoretype JKS -alias 1

