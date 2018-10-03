#!/bin/bash

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
    export HOMEBREW_CASK_OPTS="--appdir=${HOME}/Applications"

    brew install git

    if [ ! -d "${HOME}/.gnupg" ]; then
        mkdir "${HOME}/.gnupg"
        touch "${HOME}/.gnupg/gpg-agent.conf"
    fi

    if [ x"" == x"$(brew ls --versions pinentry-mac)" ]; then
        brew install pinentry-mac
        echo "pinentry-program ${HOMEBREW}/bin/pinentry-mac" >> "${HOME}/.gnupg/gpg-agent.conf"
    fi
    [ x"" == x"$(brew ls --versions gnupg       )" ] && brew install gnupg

elif [ "$(uname)" == "Linux" ]; then
    if [ -d /etc/redhat-release ]; then
        sudo yum install git
    elif [ -f /etc/debian_version ]; then
        sudo -S apt-get install -y git
    elif [ -f /etc/arch_release ]; then
        sudo pacman -Sy git
    fi
fi

pushd "${HOME}"

git clone https://github.com/alyssackwan/.password-store.git

cp -r .password-store/.gnupg .
cp -r .password-store/.ssh .

pushd .gnupg
gpg --output secret-keys.asc --decrypt secret-keys.asc.gpg
gpg --import secret-keys.asc
rm secret-keys.asc
popd

pushd .ssh
gpg --output id_rsa --decrypt id_rsa.gpg
popd

rm -rf .password-store
git clone git@github.com:alyssackwan/.password-store.git

rm -rf .ssh
rm -rf .gnupg

ln -s .password-store/.gnupg .
ln -s .password-store/.ssh .

pushd .gnupg
gpg --output secret-keys.asc --decrypt secret-keys.asc.gpg
gpg --import secret-keys.asc
rm secret-keys.asc
popd

pushd .ssh
gpg --output id_rsa --decrypt id_rsa.gpg
popd

popd
