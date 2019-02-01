#!/bin/bash


[ -z "$PS1" ] && return
[[ $- != *i* ]] && return

if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi


CURRENT_PYTHON_VERSION='3.7.0'
CURRENT_PYTHON_VERSION_REGEX='3\.7\.0'
CURRENT_NODE_VERSION='stable'

## macOS Installation
if [ "$(uname)" == "Darwin" ]; then

    # pkgconfig: C/C++ compilation
    export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig:${PKG_CONFIG_PATH}"

    # Homebrew: env var to allow shell to find Homebrew, etc.
    export HOMEBREW="${HOME}/opt/homebrew"
    export HOMEBREW_CACHE="${HOME}/Library/Caches/Homebrew"
    export PATH="$HOMEBREW/bin:$HOMEBREW/sbin:$PATH"
    export PKG_CONFIG_PATH="$HOMEBREW/lib/pkgconfig:$PKG_CONFIG_PATH"
    export CPATH="$CPATH:$HOMEBREW/include"
    export HOMEBREW_CASK_OPTS="--appdir=${HOME}/Applications"

    # coreutils: set of C/C++ compilation tool dependency 
    [ x"" == x"$(brew ls --versions coreutils)" ] && brew install coreutils
    # coreutils - man
    export "MANPATH=$MANPATH:$HOMEBREW/opt/coreutils/libexec/gnuman"

## Linux Installations
elif [ "$(uname)" == "Linux" ]; then

    export PATH="${PATH}:/root/.local/bin"

    # When I want Debian, I'll need this; until then, unneeded
    #password=$(gpg --quiet --decrypt "${HOME}/.gnupg/.password.gpg")

fi

# Calculate installed_cache for each OS: installed_cache holds a newline separated list of all packages installed in the system for faster lookup process
if [ "$(uname)" == "Darwin" ]; then
    installed_cache="$(brew ls)"
elif [ "$(uname)" == "Linux" ]; then
    if [ -d /etc/redhat-release ]; then
        installed_cache="$(rpm -qa)"
    elif [ -f /etc/debian_version ]; then
        installed_cache="$(dpkg-query -f '${binary:Package}\n' -W)"
    elif [ -f /etc/arch_release ]; then
        installed_cache="$(pacman -Qqe)"
    else
        installed_cache=""
    fi
else
    installed_cache=""
fi

# Uses installed_cache to return a binary status code re existence of package
installedp() {
    local installed=$(echo "${installed_cache}" | egrep ^"${1}"\(-\([0-9.]+\|dev\)\)?\(:amd64\)?$)
    [ x"" != x"${installed}" ]
}

# If package not found, install it
install() {
    installedp "${1}"
    if [ $? -ne 0 ]; then
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
install_array() {
    installedp "${1}"
    if [ $? -ne 0 ]; then
        if [ "$(uname)" == "Darwin" ]; then
            brew install "${2[@]}"
        elif [ "$(uname)" == "Linux" ]; then
            if [ -d /etc/redhat-release ]; then
                sudo yum install "${2[@]}"
            elif [ -f /etc/debian_version ]; then
                echo "${password}" | sudo -S apt-get install -y "${2[@]}"
            elif [ -f /etc/arch_release ]; then
                sudo pacman -Sy "${2[@]}"
            fi
        fi
    fi
}

# bash-completion: expanded functionality for tab complete
### Must be installed before the following tools! Otherwise, bash-completion can't pick up the hooks for that package, reducing its functionality
install bash-completion bash-completion
if [ "$(uname)" == "Darwin" ]; then
    source $HOMEBREW/etc/bash_completion
elif [ "$(uname)" == "Linux" ]; then
    source /usr/share/bash-completion/bash_completion
fi

# Wraps gpg-agent to make it easier to work with: gpg-agent lets you log into ssh keys so they can be used repeatedly without having to re-enter the password 
### Done for both macOS and Linux
install keychain keychain
if [ "$(uname)" == "Darwin" ]; then
    eval `keychain --eval --agents ssh --inherit any id_rsa`
elif [ "$(uname)" == "Linux" ]; then
    eval `keychain --eval --agents ssh id_rsa`
fi

# Some people don't use Git! Install 'hg' to interact with those codebases
### Mercurial was giving us sh*t the day we tried, skipped it as well as Beancount
#install mercurial mercurial

# Wraps curl to handle downloading files with greater ease and persistence: curl requires specifying vars with each download and breaks if the connection is interrupted 
install wget      wget
if [ "$(uname)" == "Linux" ]; then
    install software-properties-common software-properties-common
    install dirmngr dirmngr
fi

# Install direnv for local env var setups
install direnv    direnv

# Convert Markdown files into PDF: built on Haskell, which takes 1.5hrs to install lol
# install pandoc    pandoc

# Mysterious artifact Alyssa needed when working with Python in Debian
#if [ "$(uname)" == "Linux" ]; then
#    if [ -f /etc/debian_version ]; then
#        install libbz2-dev      libbz2-dev
#        install libreadline-dev libreadline-dev
#        install libsqlite3-dev  libsqlite3-dev
#    fi
#fi

# Haskell: not currently needed
#if [ "$(uname)" == "Darwin" ]; then
#    install haskell-stack haskell-stack
#elif [ "$(uname)" == "Linux" ]; then
#    if [ -d /etc/arch_release ] ; then
#        install stack stack
#    else
#        if [ ! -d "${HOME}/bin/stack" ]; then
#            mkdir -p "${HOME}/tmp"
#            rm -rf "${HOME}/tmp/stack"
#            curl -sSL 'https://get.haskellstack.org/' > "${HOME}/tmp/stack"
#            echo "${password}" | sudo -S sh "${HOME}/tmp/stack" -d "${HOME}/bin/stack"
#            rm -rf "${HOME}/tmp/stack"
#        fi
#        export PATH="${PATH}:${HOME}/bin/stack"
#    fi
#fi

# installedp opam
#if [ $? -ne 0 ]; then
#    install opam opam
#    opam init --enable-shell-hook
#fi
#eval `opam config env`
#eval $(opam env)

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
    # install pyenv-virtualenvwrapper pyenv-virtualenvwrapper
elif [ "$(uname)" == "Linux" ]; then
    install python-pip python-pip
    if [ ! -d "${HOME}/.pyenv" ]; then
        git clone https://github.com/pyenv/pyenv.git "${HOME}/.pyenv"
        export PYENV_ROOT="${HOME}/.pyenv"
        export PATH="${PYENV_ROOT}/bin:${PATH}"
        git clone https://github.com/pyenv/pyenv-virtualenv.git "$(pyenv root)/plugins/pyenv-virtualenv"
        # git clone https://github.com/pyenv/pyenv-virtualenvwrapper.git "$(pyenv root)/plugins/pyenv-virtualenvwrapper"
    else
        export PYENV_ROOT="${HOME}/.pyenv"
        export PATH="${PYENV_ROOT}/bin:${PATH}"
    fi
fi
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
current_python_version_match=$(pyenv versions | grep "^  ${CURRENT_PYTHON_VERSION_REGEX}$")
[ ! "${current_python_version_match}" == "  ${CURRENT_PYTHON_VERSION}" ] && pyenv install "${CURRENT_PYTHON_VERSION}"
pyenv global system
# pyenv virtualenvwrapper

# Node.js
export NVM_DIR="$HOME/.nvm"
if [ "$(uname)" == "Darwin" ]; then
    install nvm nvm
    mkdir -p "${NVM_DIR}"
    source "$(brew --prefix nvm)/nvm.sh"
elif [ "$(uname)" == "Linux" ]; then
    if [ ! -d "${NVM_DIR}" ]; then
        git clone https://github.com/creationix/nvm.git "${NVM_DIR}"
        pushd "${NVM_DIR}" > /dev/null
        git checkout `git describe --abbrev=0 --tags`
        popd > /dev/null
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

# Emacs
if [ "$(uname)" == "Darwin" ]; then
    emacs_pkg_with_args=("emacs" "--with-cocoa" "--with-dbus" "--with-imagemagick@6" "--with-librsvg" "--with-mailutils" "--with-modules")
    install_array emacs emacs_pkg_with_args
    [ ! -L "${HOME}/Applications/Emacs.app" ] && ln -s "${HOMEBREW}/opt/emacs/Emacs.app" "${HOME}/Applications/"
elif [ "$(uname)" == "Linux" ]; then
    if [ -f /etc/debian_version ]; then
        install emacs25 emacs25
    else
        install emacs emacs
    fi
fi

# Emacs Java plugin
if [ ! -d "${HOME}/.emacs.d/eclipse.jdt.ls/server/" ]; then
    mkdir -p "${HOME}/.emacs.d/eclipse.jdt.ls/server/"
    wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz -O /tmp/jdt-latest.tar
    tar xf /tmp/jdt-latest.tar -C ~/.emacs.d/eclipse.jdt.ls/server/
fi

# Beancount: emacs-based double-entry bookkeeping
#if [ ! -d "${HOME}/.beancount" ]; then
#    hg clone https://bitbucket.org/blais/beancount "${HOME}/.beancount"
#    pushd "${HOME}/.beancount" > /dev/null
#    pyenv virtualenv 3.7.0 beancount
#    pyenv shell beancount
#    pip install .
#    pyenv shell --unset
#    popd > /dev/null
#fi

# Mysterious artifact: sox is a sound formatting/parsing plugin for Linux that Alyssa once needed; keeping bc shrug
install sox sox
if [ "$(uname)" == "Linux" ]; then
    install libsox-fmt-all libsox-fmt-all
fi

# Add bin_platform and bin to PATH
export PATH="${HOME}/bin_platform:${HOME}/bin:${PATH}"
# Add bin_local before ^^: this order establishes a precedence hierarchy so that the shell finds and uses binaries in a way that allows for setting overrides (local comes before platform comes before bin)
export PATH="${HOME}/bin_local:${PATH}"

[ -f ~/.bashrc_local ] && source ~/.bashrc_local

# Direnv - Last
eval "$(direnv hook bash)"
