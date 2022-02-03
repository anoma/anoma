#!/usr/bin/env bash

# Make a release archive from built Anoma binaries and dylib(s)

set -e

VERSION="$(git describe --dirty --broken)"
PLATFORM="$(uname -s)-$(uname -m)"
PACKAGE_NAME="anoma-${VERSION}-${PLATFORM}"
BIN="anoma anomac anoman anomaw"

mkdir -p ${PACKAGE_NAME} && \
cd target/release && \
MM_TOKEN_EXCH=$(find . -maxdepth 1 -name 'libmm_token_exch.so' -or -name 'libmm_token_exch.dll' -or -name 'libmm_token_exch.dylib') && \
ln ${BIN} ${MM_TOKEN_EXCH} ../../${PACKAGE_NAME} && \
cd ../.. && \
tar -c -z -f ${PACKAGE_NAME}.tar.gz ${PACKAGE_NAME} && \
rm -rf ${PACKAGE_NAME}
