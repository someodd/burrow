#!/bin/sh
nohup /usr/sbin/sshd -D >/dev/null 2>&1 &
burrow serve --config /tmp/data/burrow.toml &
tail -f /dev/null