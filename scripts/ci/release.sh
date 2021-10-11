# script used only during CI execution.

# setup git
gh auth login --with-token $GITHUB_TOKEN

# check if its an annotated tag (https://git-scm.com/book/en/v2/Git-Basics-Tagging)
TAG_DATA=$(git show ${DRONE_TAG})
TAGGER=$(echo "$TAG_DATA" | sed -n 2p | cut -c-6)

RELEASE="0"

if [ "$TAGGER" == "Tagger" ]; then
    RELEASE="1"
fi


if [ "$RELEASE" -eq "1"]; then
  gh release create ${DRONE_TAG} -F CHANGELOG.md ./dist/*.tar.gz --target $DRONE_COMMIT_SHA
else
  gh release create ${DRONE_TAG} -F CHANGELOG.md ./dist/*.tar.gz --prerelease --target $DRONE_COMMIT_SHA
fi