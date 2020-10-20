#!/bin/bash

target_pem="${1}"

if [[ ! -f "${target_pem}" ]]; then
    echo "Usage: $0 BASE64_CERTIFICATE_FILE" >&2
    exit 1
fi

openssl x509 -subject -issuer -noout -dates -in "$target_pem"

openssl verify -purpose sslclient -CAfile ./testCA/cacert.pem "$target_pem"

