#!/bin/sh -ex

if [ ! -f netcipher-2.1.0.jar ]; then
    wget https://dl.bintray.com/guardianproject/CipherKit/info/guardianproject/netcipher/netcipher/2.1.0/netcipher-2.1.0.jar
    mv netcipher-2.1.0.jar info.gardianproject.netcipher.jar
else
    echo netcipher-2.1.0.jar already downloaded
fi
