#!/bin/bash

# Darwin
if [ "$(uname)" == "Darwin" ]; then

    # make sure opt exists
    if [ ! -d ~/opt ]; then
        mkdir ~/opt
    fi

    # pkgconfig
    export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig:$PKG_CONFIG_PATH

    # Homebrew
    if [ ! -d ~/opt/homebrew ]; then
        mkdir ~/opt/homebrew
        (cd ~/opt && curl -L https://github.com/Homebrew/homebrew/tarball/master | tar xz --strip 1 -C homebrew)
        ~/opt/homebrew/bin/brew update
    fi
    export HOMEBREW=~/opt/homebrew
    export HOMEBREW_CACHE=~/Library/Caches/Homebrew
    export PATH=$HOMEBREW/bin:$PATH
    export PKG_CONFIG_PATH=$HOMEBREW/lib/pkgconfig:$PKG_CONFIG_PATH

    [ x"" == x"$(brew ls --versions bash-completion)" ] && brew install bash-completion
    [ x"" == x"$(brew ls --versions git            )" ] && brew install git

    source $HOMEBREW/etc/bash_completion

# Linux
elif [ "$(uname)" == "Linux" ]; then

    # rpm
    if [ -d /etc/redhat-release ]; then

        [ x"" == x"$(rpm -qa | grep bash-completion-)" ] && sudo yum install bash-completion
        [ x"" == x"$(rpm -qa | grep git-            )" ] && sudo yum install git

    elif [ -d /etc/debian_version ]; then

        [ ! $(dpkg-query -Wf'${db:Status-abbrev}' bash-completion 2>/dev/null | grep -q '^i') ] && sudo apt-get install -y bash-completion
        [ ! $(dpkg-query -Wf'${db:Status-abbrev}' git             2>/dev/null | grep -q '^i') ] && sudo apt-get install -y git

    fi

    source /usr/share/bash-completion/bash_completion

fi

if [ ! -d ~/bin ]; then
    mkdir ~/bin
fi
export PATH=~/bin:$PATH
