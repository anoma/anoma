cargo = $(env) cargo

build:
	mdbook build

serve:
	mdbook serve --open

dev-deps:
	$(cargo) install mdbook
	$(cargo) install mdbook-mermaid \
		--git https://github.com/eminence/mdbook-mermaid --branch bump_mdbook --rev 4f33cfa917832b21477e858380cdff2ace61baf3
    # TODO ^ temporary until https://github.com/badboy/mdbook-mermaid/pull/11 is merged and released

.PHONY: build serve
