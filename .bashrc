#!/bin/bash

# postgresql postgresql-client
# apache2
# sudo mv /etc/apache2/mods-enabled/mpm_event.conf /etc/apache2/mods-enabled/mpm_event.conf.bak
# sudo mv /etc/apache2/mods-enabled/mpm_event.load /etc/apache2/mods-enabled/mpm_event.load.bak
# sudo ln -s /etc/apache2/mods-available/mpm_prefork.conf /etc/apache2/mods-enabled/
# sudo ln -s /etc/apache2/mods-available/mpm_prefork.load /etc/apache2/mods-enabled/
# sudo service apache2 restart
# libapache2-mod-php
# php -m | grep -i -e '^ctype$'
# ctype
# curl php-curl
# dom php-xml
# gd php-gd
# hash
# iconv
# intl php-intl
# json
# libxml
# mbstring php-mbstring
# openssl
# PDO php-pdo
# Phar
# posix
# SimpleXML
# xmlwriter
# zip php-zip
# zlib
# pgsql php-pgsql
# wget -nv https://download.owncloud.org/download/repositories/10.0/Debian_9.0/Release.key -O /tmp/Release.key
# sudo apt-key add - < /tmp/Release.key
# echo 'deb http://download.owncloud.org/download/repositories/10.0/Debian_9.0/ /' | sudo tee /etc/apt/sources.list.d/owncloud.list
# sudo apt-get update
# sudo apt-get install owncloud-files

[ -z "$PS1" ] && return
[[ $- != *i* ]] && return

if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

CURRENT_NODE_VERSION='v7.4.0'

eval `ssh-agent -s`

if [ "$(uname)" == "Darwin" ]; then

    # make sure opt exists
    if [ ! -d "${HOME}/opt" ]; then
        mkdir "${HOME}/opt"
    fi

    # pkgconfig
    export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig:${PKG_CONFIG_PATH}"

    # Homebrew
    if [ ! -d "${HOME}/opt/homebrew" ]; then
        mkdir "${HOME}/opt/homebrew"
        (cd ~/opt && curl -L https://github.com/Homebrew/homebrew/tarball/master | tar xz --strip 1 -C homebrew)
        "${HOME}/opt/homebrew/bin/brew" update
    fi
    export HOMEBREW="${HOME}/opt/homebrew"
    export HOMEBREW_CACHE="${HOME}/Library/Caches/Homebrew"
    export PATH="$HOMEBREW/bin:$HOMEBREW/sbin:$PATH"
    export PKG_CONFIG_PATH="$HOMEBREW/lib/pkgconfig:$PKG_CONFIG_PATH"
    export CPATH="$CPATH:$HOMEBREW/include"
    # export DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$HOMEBREW/lib"
    export HOMEBREW_CASK_OPTS="--appdir=${HOME}/Applications"

    # coreutils
    [ x"" == x"$(brew ls --versions coreutils)" ] && brew install coreutils
    # coreutils - man
    export "MANPATH=$MANPATH:$HOMEBREW/opt/coreutils/libexec/gnuman"

    if [ ! -d "${HOME}/.gnupg" ]; then
        mkdir "${HOME}/.gnupg"
        touch "${HOME}/.gnupg/gpg-agent.conf"
    fi
    if [ x"" == x"$(brew ls --versions pinentry-mac)" ]; then
        brew install pinentry-mac
        echo "pinentry-program ${HOMEBREW}/bin/pinentry-mac" >> "${HOME}/.gnupg/gpg-agent.conf"
    fi
    [ x"" == x"$(brew ls --versions gnupg       )" ] && brew install gnupg

    if [ ! -d "${HOME}/Applications" ]; then
        mkdir "${HOME}/Applications"
    fi

elif [ "$(uname)" == "Linux" ]; then

    password=$(gpg --quiet --decrypt "${HOME}/.gnupg/.password.gpg")

fi

if [ "$(uname)" == "Darwin" ]; then
    installed="$(brew ls)"
elif [ "$(uname)" == "Linux" ]; then
    if [ -d /etc/redhat-release ]; then
        installed="$(rpm -qa)"
    elif [ -f /etc/debian_version ]; then
        installed="$(dpkg-query -f '${binary:Package}\n' -W)"
    elif [ -f /etc/arch_release ]; then
        installed="$(pacman -Qqe)"
    else
        installed=""
    fi
else
    installed=""
fi

install() {
    local installed=$(echo "${installed}" | grep ^"${1}"$)
    if [ x"" == x"${installed}" ]; then
        if [ "$(uname)" == "Darwin" ]; then
            brew install "${2}"
        elif [ "$(uname)" == "Linux" ]; then
            if [ -d /etc/redhat-release ]; then
                sudo yum install "${2}"
            elif [ -f /etc/debian_version ]; then
                echo "${password}" | sudo -S apt-get install -y "${2}"
            elif [ -f /etc/arch_release ]; then
                sudo pacman -Sy "${2}"
            fi
        fi
    fi
}

if [ "$(uname)" == "Linux" ]; then
    if [ -f /etc/debian_version ]; then
        install mosh mosh
    fi
fi

install bash-completion bash-completion
if [ "$(uname)" == "Darwin" ]; then
    source $HOMEBREW/etc/bash_completion
elif [ "$(uname)" == "Linux" ]; then
    source /usr/share/bash-completion/bash_completion
fi

install git       git
install mercurial mercurial
install wget      wget
if [ "$(uname)" == "Linux" ]; then
    install dirmngr dirmngr
fi
install tmux      tmux
install direnv    direnv
install pandoc    pandoc

if [ "$(uname)" == "Linux" ]; then
    if [ -f /etc/debian_version ]; then
        install libbz2-dev      libbz2-dev
        install libreadline-dev libreadline-dev
        install libsqlite3-dev  libsqlite3-dev
    fi
fi

# Haskell
if [ "$(uname)" == "Darwin" ]; then
    install haskell-stack haskell-stack
elif [ "$(uname)" == "Linux" ]; then
    if [ -d /etc/arch_release ] ; then
        install stack stack
    else
        [ ! -d "${HOME}/bin/stack" ] && curl -sSL https://get.haskellstack.org/ | sh -s - -d "$HOME/bin/stack"
    fi
fi

# Go
if [ "$(uname)" == "Darwin" ]; then
    install go go
elif [ "$(uname)" == "Linux" ]; then
    if [ -f /etc/debian_version ]; then
        install golang golang
    else
        install go go
    fi
fi
export GOPATH="${HOME}/Projects/go"
export PATH="${PATH}:${GOPATH}/bin"
[ ! -d "${GOPATH}/src/golang.org/x/tools/cmd"        ] && go get -u golang.org/x/tools/cmd/...
[ ! -d "${GOPATH}/src/github.com/kardianos/govendor" ] && go get -u github.com/kardianos/govendor
[ ! -d "${GOPATH}/src/github.com/nsf/gocode"         ] && go get -u github.com/nsf/gocode
[ ! -d "${GOPATH}/src/github.com/rogpeppe/godef"     ] && go get -u github.com/rogpeppe/godef

# Python
if [ "$(uname)" == "Darwin" ]; then
    install pyenv                   pyenv
    install pyenv-virtualenv        pyenv-virtualenv
    install pyenv-virtualenvwrapper pyenv-virtualenvwrapper
elif [ "$(uname)" == "Linux" ]; then
    install python-pip python-pip
    if [ ! -d "${HOME}/.pyenv" ]; then
        git clone https://github.com/pyenv/pyenv.git "${HOME}/.pyenv"
        export PYENV_ROOT="${HOME}/.pyenv"
        export PATH="${PYENV_ROOT}/bin:${PATH}"
        git clone https://github.com/pyenv/pyenv-virtualenv.git "$(pyenv root)/plugins/pyenv-virtualenv"
        git clone https://github.com/pyenv/pyenv-virtualenvwrapper.git "$(pyenv root)/plugins/pyenv-virtualenvwrapper"
    else
        export PYENV_ROOT="${HOME}/.pyenv"
        export PATH="${PYENV_ROOT}/bin:${PATH}"
    fi
fi
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
pyenv install 
pyenv global system
pyenv virtualenvwrapper

# Java
if [ ! -d "${HOME}/.emacs.d/eclipse.jdt.ls/server/" ]; then
    mkdir -p "${HOME}/.emacs.d/eclipse.jdt.ls/server/"
    wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz -O /tmp/jdt-latest.tar
    tar xf /tmp/jdt-latest.tar -C ~/.emacs.d/eclipse.jdt.ls/server/
fi

# Node.js
export NVM_DIR="$HOME/.nvm"
if [ "$(uname)" == "Darwin" ]; then
    install nvm nvm
    mkdir -p "${NVM_DIR}"
    source "$(brew --prefix nvm)/nvm.sh"
elif [ "$(uname)" == "Linux" ]; then
    if [ ! -d "${NVM_DIR}" ]; then
        git clone https://github.com/creationix/nvm.git "${NVM_DIR}"
        pushd "${NVM_DIR}"
        git checkout `git describe --abbrev=0 --tags`
        popd
    fi
    source "${NVM_DIR}/nvm.sh"
fi
[ ! $(nvm version node | grep "${CURRENT_NODE_VERSION}") ] && nvm install "${CURRENT_NODE_VERSION}" && nvm alias default "${CURRENT_NODE_VERSION}"
[ $(npm list --depth 0 --global tern > /dev/null 2>&1) ] && npm install -g tern

# Ruby
if [ "$(uname)" == "Darwin" ]; then
    install rbenv      rbenv
    install ruby-build ruby-build
elif [ "$(uname)" == "Linux" ]; then
    if [ ! -d "${HOME}/.rbenv" ]; then
        git clone https://github.com/rbenv/rbenv.git "${HOME}/.rbenv"
    fi
    export PATH="${PATH}:${HOME}/.rbenv/bin"
fi
eval "$(rbenv init -)"

# Miscellaneous

# pass
install pass pass
if [ "$(uname)" == "Darwin" ]; then
    install lastpass-cli "lastpass-cli --with-pinentry"
elif [ "$(uname)" == "Linux" ]; then
    if [ -d /etc/redhat-release ] || [ -f /etc/arch_release ]; then
        install lastpass-cli lastpass-cli
    elif [ -f /etc/debian_version ]; then
        if [ ! -d "${HOME}/.lastpass-cli" ]; then
            sudo apt-get --no-install-recommends -yqq install \
              bash-completion \
              build-essential \
              cmake \
              libcurl3  \
              libcurl3-openssl-dev  \
              libssl1.0 \
              libssl1.0-dev \
              libxml2 \
              libxml2-dev  \
              pkg-config \
              ca-certificates \
              xclip
            git clone https://github.com/lastpass/lastpass-cli.git "${HOME}/.lastpass-cli"
            pushd "${HOME}/.lastpass-cli"
            git checkout `git describe --abbrev=0 --tags`
            make
            sudo make install
            popd
        fi
    fi
fi

# Emacs
if [ "$(uname)" == "Darwin" ]; then
    install emacs "emacs --with-cocoa --with-dbus --with-imagemagick@6 --with-librsvg --with-mailutils --with-modules"
    [ ! -L "${HOME}/Applications/Emacs.app" ] && ln -s "${HOMEBREW}/opt/emacs/Emacs.app" "${HOME}/Applications/"
elif [ "$(uname)" == "Linux" ]; then
    if [ -f /etc/debian_version ]; then
        install emacs25 emacs25
    else
        install emacs emacs
    fi
fi

# offlineimap + mu
if [ "$(uname)" == "Darwin" ]; then
    install imagemagick imagemagick
fi
install w3m         w3m
install offlineimap offlineimap
if [ "$(uname)" == "Darwin" ]; then
    install mu mu
elif [ "$(uname)" == "Linux" ]; then
    install maildir-utils maildir-utils
    install mu4e          mu4e
fi

# Ledger
install ledger ledger
# Beancount
[ ! -d "${HOME}/.beancount" ] && hg clone https://bitbucket.org/blais/beancount "${HOME}/.beancount"
pushd "${HOME}/.beancount"
pyenv shell 3.7.0
pip install .
pyenv shell --unset
popd

install sox sox
if [ "$(uname)" == "Linux" ]; then
    install libsox-fmt-all libsox-fmt-all
fi


if [ ! -d "${HOME}/bin_local" ]; then
    mkdir "${HOME}/bin_local"
fi
export PATH="${HOME}/bin_local:${HOME}/bin:${PATH}"

[ -f ~/.bashrc_local ] && source ~/.bashrc_local

# Direnv - Last
eval "$(direnv hook bash)"
