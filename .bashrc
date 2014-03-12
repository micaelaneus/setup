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
elif [ "$(uname)" == "Linux" ]; then
    if [ -d /etc/redhat-release ]; then
        if  [ x"" == x"$(rpm -qa | grep gnupg2-         )" ] ||
            [ x"" == x"$(rpm -qa | grep pass-           )" ] ||
            [ x"" == x"$(rpm -qa | grep bash-completion-)" ] ||
            [ x"" == x"$(rpm -qa | grep git-            )" ]; then
            su - root
            [ x"" == x"$(rpm -qa | grep gnupg2-         )" ] && yum install gnupg2
            [ x"" == x"$(rpm -qa | grep pass-           )" ] && yum install pass
            [ x"" == x"$(rpm -qa | grep bash-completion-)" ] && yum install bash-completion
            [ x"" == x"$(rpm -qa | grep git-            )" ] && yum install git
            exit
        fi
    fi
fi

# bash_completion
if [ "$(uname)" == "Darwin" ]; then
    source $HOMEBREW/etc/bash_completion
elif [ "$(uname)" == "Linux" ]; then
    source /usr/share/bash-completion/bash_completion
fi

if [ ! -d ~/bin ]; then
    mkdir ~/bin
fi
export PATH=~/bin:$PATH
