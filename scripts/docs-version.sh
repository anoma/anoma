version=`mix run -e 'IO.puts(Mix.Project.config()[:version])'`; \
mix docs -o "doc/v$version"

# hack to get around github pages being a bit stupid
for v in $(git tag | tac); do mkdir -p "./doc/$v" &&  cp ./doc/.doc-versions.js "./doc/$v/.doc-versions.js"; done
