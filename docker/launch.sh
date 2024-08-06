#!/bin/sh
nohup /usr/sbin/sshd -D >/dev/null 2>&1 &
spacecookie /etc/spacecookie.json
