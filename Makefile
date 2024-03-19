build:
	mix compile

test:
	mix test

docs:
	./scripts/docs.sh

.PHONY: build test
