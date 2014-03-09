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
        ~/opt/homebrew/bin/brew update
    fi
    # load
    export HOMEBREW=~/opt/homebrew
    export HOMEBREW_CACHE=~/Library/Caches/Homebrew
    export PATH=$HOMEBREW/bin:$PATH
fi

# install
if [ "$(uname)" == "Darwin" ]; then
    [ x"" == x"$(brew ls --versions gnupg2         )" ] && brew install gnupg2
    [ x"" == x"$(brew ls --versions pass           )" ] && brew install pass
    [ x"" == x"$(brew ls --versions bash-completion)" ] && brew install bash-completion
    [ x"" == x"$(brew ls --versions git            )" ] && brew install git
fi

# bash_completion
. $HOMEBREW/etc/bash_completion

if [ ! -d ~/bin ]; then
    mkdir ~/bin
fi
export PATH=~/bin:$PATH
