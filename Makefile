CWD != pwd
# 2.20 a somewhat arbitrary choice for an oldish glibc version
ENV = CFLAGS='-target x86_64-linux-gnu.2.20' CXXFLAGS='-target x86_64-linux-gnu.2.20'

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
	mix toc
	./scripts/docs.sh
	./scripts/docs-files.sh

docs-release:
	make docs
	./scripts/docs-version.sh

keygen: key.pem

key.pem:
	openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:secp384r1 -days 3650 -nodes -keyout key.pem -out cert.pem -subj /CN=none

.PHONY: default nothing keygen build test docs
