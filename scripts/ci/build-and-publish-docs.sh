#!/bin/sh
# script used only during CI execution.

set -e

PUSH_URL="https://${GITHUB_TOKEN}@github.com/anoma/anoma.git"

make build-doc

if [ -n "$DRONE_PULL_REQUEST" ]; then
    exit 0;
fi

mkdir -p docs/book/rustdoc
mv -v target/doc/* docs/book/rustdoc/
mv docs/book/html/* docs/book/
mkdir -p ~/.tmp/book
mv -v docs/book/* ~/.tmp/book/
cd ~

git clone "$DRONE_GIT_HTTP_URL" --branch gh-pages --single-branch anoma-docs && cd anoma-docs
git config user.name "AnomaBot"
git config user.email "gianmarco@heliax.dev"

if [ -z "$DRONE_TAG" ];
then
    rm -rf master
    mkdir -p master
    mv -v ~/.tmp/book/* master/
else
    rm -rf "$DRONE_TAG"
    mkdir -p "$DRONE_TAG"
    mv -v ~/.tmp/book/* "$DRONE_TAG/"
fi

if [ -z "$(git status --porcelain)" ];
then
    echo "No changes to commit." && exit 0
fi

git remote -v
git remote set-url origin "$PUSH_URL"
git add -A
git commit -m "[docs]: update commit ${DRONE_COMMIT}"
git push --set-upstream origin gh-pages
