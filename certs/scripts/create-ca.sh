#!/bin/sh
BASE_DIR=./testCA

#create target directories, set permissions
mkdir -p $BASE_DIR/private
chmod go-rx $BASE_DIR/private

#generate the CA certificate
openssl req -batch -x509 -days 3000 -config openssl-ca.cnf -newkey rsa:4096 -sha256 -out cacert.pem -outform PEM

#move generated certificates into their proper places
mv cacert.pem $BASE_DIR
mv cakey.pem $BASE_DIR/private

#init the certificate database files
touch $BASE_DIR/index.txt
echo '01' > $BASE_DIR/serial.txt

mkdir -p $BASE_DIR/newcerts

