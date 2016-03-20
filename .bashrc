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

    [ x"" == x"$(brew ls --versions bash-completion)" ] && brew install bash-completion
    [ x"" == x"$(brew ls --versions git            )" ] && brew install git

    source $HOMEBREW/etc/bash_completion

    if [ x"" == x"$(brew ls --versions pyenv)" ]; then
        brew install pyenv
        brew install pyenv-virtualenv
        eval "$(pyenv init -)"
        eval "$(pyenv virtualenv-init -)"
        pyenv install 3.5.0 && pyenv global 3.5.0
    else
        eval "$(pyenv init -)"
        eval "$(pyenv virtualenv-init -)"
    fi

elif [ "$(uname)" == "Linux" ]; then

    if [ -d /etc/redhat-release ]; then

        [ x"" == x"$(rpm -qa | grep bash-completion-)" ] && sudo yum install bash-completion
        [ x"" == x"$(rpm -qa | grep git-            )" ] && sudo yum install git

    elif [ -d /etc/debian_version ]; then

        [ ! $(dpkg-query -Wf'${db:Status-abbrev}' bash-completion 2>/dev/null | grep -q '^i') ] && sudo apt-get install -y bash-completion
        [ ! $(dpkg-query -Wf'${db:Status-abbrev}' git             2>/dev/null | grep -q '^i') ] && sudo apt-get install -y git

    elif [ -f /etc/arch_release ]; then

        ! sudo pacman -Q bash-completion && sudo pacman -Sy bash-completion
        ! sudo pacman -Q git             && sudo pacman -Sy git

    fi

    source /usr/share/bash-completion/bash_completion

    if [ ! -d ~/.pyenv ]; then
        git clone https://github.com/yyuu/pyenv.git ~/.pyenv
        git clone https://github.com/yyuu/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv
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

fi

if [ ! -d ~/bin ]; then
    mkdir ~/bin
fi
export PATH=~/bin:$PATH
