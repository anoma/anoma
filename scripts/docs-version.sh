#!/bin/bash

version=$(mix run -e 'IO.puts(to_string(Mix.Project.config()[:version]))')
mix docs -o "doc/v$version"

# hack to get around github pages being a bit stupid
for v in $(git tag | tac 2>/dev/null || git tag | tail -r); do
    mkdir -p "./doc/$v"
    [ -f ./doc/.doc-versions.js ] && cp ./doc/.doc-versions.js "./doc/$v/.doc-versions.js"
done
