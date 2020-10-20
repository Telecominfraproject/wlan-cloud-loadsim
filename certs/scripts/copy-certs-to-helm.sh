#!/bin/bash
# Script to copy certs to the respective folders in wlan-cloud-helm folders. 
# Make sure you are in wlan-pki-folder with generated
# Usage: ./copy-certs.sh ${wlan-cloud-helm-dir}
# ./copy-certs.sh $HOME/Tip-Repo/wlan-cloud-helm

if [[ $# -eq 0 ]] ;
then
  echo "*** No Arguments supplied!! Expecting Absolute path of wlan-cloud-helm dir as an argument to the script ***"
  echo "*** Usage: ./copy-certs.sh absolute-path-of-wlan-cloud-helm-dir ***"
  exit 0
fi
echo "==============================================="
echo "Copying certs to opensync-gw-cloud certs folder"
cp cacert.pem clientcert.pem clientkey.pem client_keystore.jks server.pkcs12 truststore.jks "$1"/tip-wlan/charts/opensync-gw-cloud/resources/config/certs
echo "================================================"
echo "Copying certs to opensync-gw-static certs folder"
cp client_keystore.jks server.pkcs12 truststore.jks "$1"/tip-wlan/charts/opensync-gw-static/resources/config/certs
echo "=================================================="
echo "Copying certs to opensync-mqtt-broker certs folder"
cp cacert.pem mqttservercert.pem mqttserverkey_dec.pem "$1"/tip-wlan/charts/opensync-mqtt-broker/resources/config/certs/
echo "====================================================================="
echo "Copying certs to wlan-integrated-cloud-component-service certs folder"
cp client_keystore.jks server.pkcs12 truststore.jks "$1"/tip-wlan/charts/wlan-integrated-cloud-component-service/resources/config/certs/
echo "================================================="
echo "Copying certs to wlan-portal-service certs folder"
cp client_keystore.jks server.pkcs12 truststore.jks "$1"/tip-wlan/charts/wlan-portal-service/resources/config/certs/
echo "==============================================="
echo "Copying certs to wlan-prov-service certs folder"
cp client_keystore.jks server.pkcs12 truststore.jks cacert.pem postgresclientcert.pem postgresclientkey_dec.pem postgresclient.p12 "$1"/tip-wlan/charts/wlan-prov-service/resources/config/certs/
echo "=============================================="
echo "Copying certs to wlan-ssc-service certs folder"
cp client_keystore.jks server.pkcs12 kafka-server.pkcs12 truststore.jks cacert.pem cassandraserverkey_dec.pem cassandraservercert.pem cassandra_server_keystore.jks "$1"/tip-wlan/charts/wlan-ssc-service/resources/config/certs/
echo "=============================================="
echo "Copying certs to wlan-spc-service certs folder"
cp client_keystore.jks server.pkcs12 kafka-server.pkcs12 truststore.jks "$1"/tip-wlan/charts/wlan-spc-service/resources/config/certs/
echo "================================================="
echo "Copying certs to wlan-port-forwarding-gateway-service certs folder"
cp client_keystore.jks server.pkcs12 truststore.jks "$1"/tip-wlan/charts/wlan-port-forwarding-gateway-service/resources/config/certs/
echo "==================================="
echo "Copying certs to kafka certs folder"
cp kafka-server.pkcs12 truststore.jks "$1"/tip-wlan/charts/kafka/resources/config/certs/
echo "======================================="
echo "Copying certs to cassandra certs folder"
cp cassandra_server_keystore.jks truststore.jks cacert.pem cassandraserverkey_dec.pem cassandraservercert.pem "$1"/tip-wlan/charts/cassandra/resources/config/certs/
echo "======================================"
echo "Copying certs to postgres certs folder"
cp cacert.pem postgresclientcert.pem postgresclientkey_dec.pem servercert.pem serverkey_dec.pem "$1"/tip-wlan/charts/postgresql/resources/config/certs/
echo "========= All Certs Copied =========="
echo "NOTE: Additional changes are expected in Kafka, Postgres and Cassandra charts before you start deployment. Refer https://telecominfraproject.atlassian.net/wiki/spaces/WIFI/pages/262176803/Pre-requisites+before+deploying+Tip-Wlan+solution"
