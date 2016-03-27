#!/bin/bash

[[ $- != *i* ]] && return

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

    # coreutils
    [ x"" == x"$(brew ls --versions coreutils      )" ] && brew install bash-completion
    # coreutils - man
    export MANPATH=$MANPATH:$HOMEBREW/opt/coreutils/libexec/gnuman

    [ x"" == x"$(brew ls --versions bash-completion)" ] && brew install bash-completion
    [ x"" == x"$(brew ls --versions git            )" ] && brew install git
    [ x"" == x"$(brew ls --versions direnv         )" ] && brew install direnv

    source $HOMEBREW/etc/bash_completion

    [ x"" == x"$(brew ls --versions go             )" ] && brew install go

    if [ x"" == x"$(brew ls --versions pyenv)" ]; then
        brew install pyenv
        brew install pyenv-virtualenv
        brew install pyenv-virtualenvwrapper
        eval "$(pyenv init -)"
        eval "$(pyenv virtualenv-init -)"
        pyenv install 3.5.0 && pyenv global 3.5.0
    else
        eval "$(pyenv init -)"
        eval "$(pyenv virtualenv-init -)"
    fi
    pyenv virtualenvwrapper
    
    if [ x"" == x"$(brew ls --versions rbenv)" ]; then
        brew install rbenv
        brew install ruby-build
    fi
    eval "$(rbenv init -)"

elif [ "$(uname)" == "Linux" ]; then

    if [ -d /etc/redhat-release ]; then

        [ x"" == x"$(rpm -qa | grep bash-completion-)" ] && sudo yum install bash-completion
        [ x"" == x"$(rpm -qa | grep git-            )" ] && sudo yum install git
        [ x"" == x"$(rpm -qa | grep go-             )" ] && sudo yum install go

    elif [ -d /etc/debian_version ]; then

        [ ! $(dpkg-query -Wf'${db:Status-abbrev}' bash-completion 2>/dev/null | grep -q '^i') ] && sudo apt-get install -y bash-completion
        [ ! $(dpkg-query -Wf'${db:Status-abbrev}' git             2>/dev/null | grep -q '^i') ] && sudo apt-get install -y git
        [ ! $(dpkg-query -Wf'${db:Status-abbrev}' go              2>/dev/null | grep -q '^i') ] && sudo apt-get install -y go

    elif [ -f /etc/arch_release ]; then

        ! sudo pacman -Q bash-completion && sudo pacman -Sy bash-completion
        ! sudo pacman -Q git             && sudo pacman -Sy git
        ! sudo pacman -Q go              && sudo pacman -Sy go

    fi

    source /usr/share/bash-completion/bash_completion

    if [ ! -d ~/.pyenv ]; then
        git clone https://github.com/yyuu/pyenv.git ~/.pyenv
        git clone https://github.com/yyuu/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv
        git clone https://github.com/yyuu/pyenv-virtualenvwrapper.git ~/.pyenv/plugins/pyenv-virtualenvwrapper
        export PYENV_ROOT="$HOME/.pyenv"
        export PATH="$PYENV_ROOT/bin:$PATH"
        eval "$(pyenv init -)"
        eval "$(pyenv virtualenv-init -)"
        pyenv install 3.5.0 && pyenv global 3.5.0
    else
        export PYENV_ROOT="$HOME/.pyenv"
        export PATH="$PYENV_ROOT/bin:$PATH"
        eval "$(pyenv init -)"
        eval "$(pyenv virtualenv-init -)"
    fi
    pyenv virtualenvwrapper
    
    if [ ! -d ~/.rbenv ]; then
        git clone https://github.com/rbenv/rbenv.git ~/.rbenv
    fi
    export PATH="$PATH:$HOME/.rbenv/bin"
    eval "$(rbenv init -)"

fi

export GOPATH="$HOME/Projects/go"
export PATH="$PATH:$GOPATH/bin"
[ ! -d "$GOPATH/src/golang.org/x/tools/cmd"        ] && go get -u golang.org/x/tools/cmd/...
[ ! -d "$GOPATH/src/github.com/kardianos/govendor" ] && go get -u github.com/kardianos/govendor

if [ ! -d ~/bin ]; then
    mkdir ~/bin
fi
export PATH=~/bin:$PATH

# Direnv - Last
if [ "$(uname)" == "Darwin" ]; then
    eval "$(direnv hook bash)"
fi
