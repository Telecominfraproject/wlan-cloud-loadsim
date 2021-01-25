#!/bin/sh

set -e

if [ ! -d ssl ]; then
	    mkdir ssl
fi

# Create the root CA (Certificate Authority)
openssl genrsa -out ssl/owls-ca.key 4096

## Certificate signing request for root CA
openssl req -x509 -new -nodes -key ssl/owls-ca.key -sha256 -days 1024 -subj "/C=SE/" -out ssl/owls-ca.pem

# Create the server certificate
openssl genrsa -out ssl/owls-server.key 4096

## Certificate signing request for server certificate
openssl req -new -key ssl/owls-server.key -subj "/C=SE/CN=localhost/" -out ssl/owls-server.csr

## Sign the server certificate using the root CA
openssl x509 -req -in ssl/owls-server.csr -CA ssl/owls-ca.pem -CAkey ssl/owls-ca.key -CAcreateserial -out ssl/owls-server.pem -days 500 -sha256
