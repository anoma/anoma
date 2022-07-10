# Introduction

Welcome to Anoma's docs!

## About Anoma

[Anoma](https://anoma.network/) is a sovereign, proof-of-stake blockchain protocol that enables private, asset-agnostic cash and private bartering among any number of parties. To learn more about the protocol, we recommend the following resources:

- [Introduction to Anoma Medium article](https://medium.com/anomanetwork/introducing-anoma-a-blockchain-for-private-asset-agnostic-bartering-dcc47ac42d9f)
- [Anoma's Whitepaper](https://anoma.network/papers/whitepaper.pdf)
- [Anoma's Vision paper](https://anoma.network/papers/vision-paper.pdf)

> ⚠️ Here lay dragons: this codebase is still experimental, try at your own risk!

## About the documentation

The two main sections of this book are:

- [Exploration](./explore): documents the process of exploring the design and implementation space for Anoma
- [Specifications](./specs): implementation independent technical specifications

The Anoma user guide and networks documentation can be found at <https://docs.anoma.net>.

### The source

This book is written using [mdBook](https://rust-lang.github.io/mdBook/) with [mdbook-mermaid](https://github.com/badboy/mdbook-mermaid) for diagrams, it currently lives in the [Anoma repo](https://github.com/anoma/anoma).

To get started quickly, in the `docs` directory one can:

```shell
# Install dependencies
make dev-deps

# This will open the book in your default browser and rebuild on changes
make serve
```

The mermaid diagrams docs can be found at <https://mermaid-js.github.io/mermaid>.

[Contributions](https://github.com/anoma/anoma/issues) to the contents and the structure of this book (nothing is set in stone) should be made via pull requests. Code changes that diverge from the spec should also update this book.
