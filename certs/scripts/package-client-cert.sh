#!/bin/sh
openssl pkcs12 -export -in clientcert.pem -inkey clientkey.pem -passin pass:mypassword -passout pass:mypassword -out client.pkcs12 -name clientqrcode -CAfile testCA/cacert.pem -caname root -chain

keytool -importkeystore -destkeystore client_keystore.jks -srckeystore client.pkcs12 -srcstoretype pkcs12 -srcstorepass mypassword -deststorepass mypassword -deststoretype JKS -alias clientqrcode

