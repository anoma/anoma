CWD != pwd
# 2.20 a somewhat arbitrary choice for an oldish glibc version
ENV = CC="$(CWD)/zig-cc" CFLAGS='-target x86_64-linux-gnu.2.20' CXX="$(CWD)/zig-c++" CXXFLAGS='-target x86_64-linux-gnu.2.20'

default: nothing
nothing:

release:
	rm -rf _build deps
	mix deps.get
	env $(ENV) mix release

build:
	mix compile

test:
	mix test

docs:
	./scripts/docs.sh

docs-release:
	make docs
	./scripts/docs-version.sh

.PHONY: build test docs
