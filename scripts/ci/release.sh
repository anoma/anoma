#!/bin/sh
# script used only during CI execution.

set -e

# setup git (maybe not needed it should be able to pick up the env)
echo "$GITHUB_TOKEN" > token.git
gh auth login --with-token < token.git

# check if its an annotated tag (https://git-scm.com/book/en/v2/Git-Basics-Tagging)
TAG_DATA=$(git show "${DRONE_TAG}")
TAGGER=$(echo "$TAG_DATA" | sed -n 2p | cut -c-6)

RELEASE="0"

if [ "$TAGGER" = "Tagger" ]; then
    RELEASE="1"
fi

if [ "$RELEASE" -eq "1" ]; then
  gh release create "${DRONE_TAG}" ./*.tar.gz --target "$DRONE_COMMIT_SHA" --draft --title "${DRONE_TAG} Release" --notes "Release ${DRONE_TAG} binaries"
else
  gh release create "${DRONE_TAG}" ./*.tar.gz --target "$DRONE_COMMIT_SHA" --draft --title "${DRONE_TAG} Pre-release" --notes "Pre-release ${DRONE_TAG} binaries"
fi
