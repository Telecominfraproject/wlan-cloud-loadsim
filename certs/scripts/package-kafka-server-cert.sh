#!/bin/sh
openssl pkcs12 -export -in kafkaservercert.pem -inkey kafkaserverkey.pem -passin pass:mypassword -passout pass:mypassword -out kafka-server.pkcs12 -name 1 -CAfile testCA/cacert.pem -caname root -chain

keytool -importkeystore -destkeystore kafka_server_keystore.jks -srckeystore kafka-server.pkcs12 -srcstoretype pkcs12 -srcstorepass mypassword -deststorepass mypassword -deststoretype JKS -alias 1

