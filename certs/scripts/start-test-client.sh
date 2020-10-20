#!/bin/sh
openssl s_client -CAfile ./testCA/cacert.pem -cert clientcert.pem -key clientkey.pem -connect 127.0.0.1:4242
