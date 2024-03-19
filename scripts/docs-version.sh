version=`mix run -e 'IO.puts(Mix.Project.config()[:version])'`; \
mix docs -o "doc/v$version"
