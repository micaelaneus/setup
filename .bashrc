#!/bin/bash

# stow
export STOW_DIR=~/stow

# make sure opt exists
if [ ! -d ~/opt ]; then
    mkdir ~/opt
fi

# homebrew
if [ "$(uname)" == "Darwin" ]; then
    # download if not exists
    if [ ! -d ~/opt/homebrew ]; then
        mkdir ~/opt/homebrew
        (cd ~/opt && curl -L https://github.com/Homebrew/homebrew/tarball/master | tar xz --strip 1 -C homebrew)
    fi
    # load
    export HOMEBREW=~/opt/homebrew
    export HOMEBREW_CACHE=~/Library/Caches/Homebrew
    export PATH=$HOMEBREW/bin:$PATH
fi

# pass
PASS_BASH_COMPLETION=$(brew list pass | grep 'bash_completion')
if [ -f $PASS_BASH_COMPLETION ]; then
    source $PASS_BASH_COMPLETION
fi

if [ ! -d ~/bin ]; then
    mkdir ~/bin
fi
export PATH=~/bin:$PATH
