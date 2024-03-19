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
