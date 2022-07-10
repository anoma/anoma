cargo = $(env) cargo

build:
	mdbook build

serve:
	mdbook serve --open

dev-deps:
	$(cargo) install mdbook
	$(cargo) install mdbook-mermaid
	$(cargo) install mdbook-linkcheck
	$(cargo) install mdbook-open-on-gh
	$(cargo) install mdbook-admonish

.PHONY: build serve
