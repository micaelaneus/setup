#!/bin/bash

eval `ssh-agent -k`

[ -f "${HOME}/.bash_logout_local" ] && . "${HOME}/.bash_logout_local"
