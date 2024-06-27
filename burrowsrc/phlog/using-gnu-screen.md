---
title: Using GNU Screen
published: 2021-06-14
updated: 2021-07-03
tags: notes linux debian server cli terminals
type: post
parentTemplate: post.txt
---

I love GNU screen. It allows me to keep various sessions open regardless if my
SSH connection terminates or not. It's also very nice for my VT320 terminal,
because I can easily switch between windows and divide up the screen. I'm told
`tmux` is better and more useful (I think for old terminals, too?), so I'll
look into that, too, at some point.

I'm making this guide for the most frequently used commands because I'm bad at
memorizing these things.

**Name and start a new screen session:** `screen -S session_name`.

**List active sessions:**: `screen -ls`

**Reconnect to a session:** `screen -r`.

**Detach from the session:** `ctrl-a` then `d`.

Other common commands:

* Ctrl+a c Command to create a new window (with shell)
* Ctrl+a " Command to list all window
* Ctrl+a 0 Switch to window 0 (by number )
* Ctrl+a A The command to rename the current window
* Ctrl+a S Command to split current region horizontally into two regions
* Ctrl+a | Split current region vertically into two regions
* Ctrl+a tab Command to switch the input focus to the next region
* Ctrl+a Ctrl+a Toggle between the current and previous region
* Ctrl+a Q Close all regions but the current one
* Ctrl+a X Command to close the current region

## Scrolling screen

Copied verbatim I think:

  1. Press "Ctrl-A" on the keyboard and press "Esc."
  1. Press the "Up" and "Down" arrow keys or the "PgUp" and "PgDn" keys to scroll through previous utput.
  1. Press "Esc" to exit scrollback mode.

## Sources

https://smallbusiness.chron.com/scroll-up-linux-screen-46302.html

https://itsubuntu.com/how-to-use-linux-gnu-screen/

https://linuxize.com/post/how-to-use-linux-screen/