#!/bin/sh
nohup /usr/sbin/sshd -D >/dev/null 2>&1 &
burrow serve --config /etc/spacecookie.json &
tail -f /dev/null