#!/usr/bin/bash

IFACE=wlan0

echo "starting testounet"
ip link set $IFACE down
iw $IFACE set type ibss
ip link set $IFACE up
iw $IFACE ibss join testounet 2437
iwconfig
