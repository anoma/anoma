#!/bin/bash

#set -x

# an examplary download-url
# https://github.com/tendermint/tendermint/releases/download/v0.34.14/tendermint_0.34.14_linux_amd64.tar.gz
export TM_MAJORMINOR="0.34"
export TM_PATCH="14"
export TM_REPO="https://github.com/tendermint/tendermint"

export TM_VERSION="${TM_MAJORMINOR}.${TM_PATCH}"

export TARGET_PATH="/usr/local/bin"
export TMP_PATH="/tmp"

error_exit()
{
    echo "Error: $1"
    exit 1
}

# check for existence
export TM_EXECUTABLE=$(which tendermint)
if [ -x "$TM_EXECUTABLE" ]; then
  export TM_EXISTS_VER=$(${TM_EXECUTABLE} version)
fi

if [[ $TM_EXISTS_VER =~ "${TM_MAJORMINOR}" ]]; then
  echo "tendermint already exists in your current PATH with a sufficient version = $TM_EXISTS_VER"
  echo "tendermint is located at = $(which tendermint)"
  exit
fi

read -r SYSTEM MACHINE <<< $(uname -s -m)

export ARCH="amd64"
if [[ $MACHINE -eq "aarch64" ]]; then
  ARCH="arm64"
fi

if [[ $MACHINE -eq "x86_64" ]]; then
  ARCH="amd64"
fi

export RELEASE_URL="${TM_REPO}/releases/download/v${TM_VERSION}/tendermint_${TM_VERSION}_$(echo ${SYSTEM} | tr '[:upper:]' '[:lower:]')_${ARCH}.tar.gz"
echo $RELEASE_URL

curl -Lo $TMP_PATH/tendermint.tar.gz $RELEASE_URL || error_exit "tendermint release download failed"

cd $TARGET_PATH
sudo tar -xvzf $TMP_PATH/tendermint.tar.gz tendermint || error_exit "tendermint release extraction failed"

