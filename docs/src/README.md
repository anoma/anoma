# Introduction

This book is written using [mdBook](https://rust-lang.github.io/mdBook/) with [mdbook-mermaid](https://github.com/badboy/mdbook-mermaid) for diagrams, it currently lives in the [Anoma repo](https://github.com/anoma/anoma).

To get started quickly, one can:

```shell
# Install dependencies
make dev-deps
# This will open the book in your default browser and rebuild on changes
make serve
```

The mermaid diagrams docs can be found at <https://mermaid-js.github.io/mermaid>.

The initial purpose of this document is twofold. One is to document the process of exploring the design and implementation space for Anoma. The other is to describe its technical specifications. These correspond to the [Exploration](./explore) and [Specifications](./specs) sections of this book, respectively.

The Exploration section is more free-form. This is largely a cross-over of both the implementation details and the design of implementation-independent specifications.

The Specification section will be completely independent of the implementation details.

Contributions to the contents and the structure of this book (nothing is set in stone) should be made via pull requests. Code changes that diverge from the spec should also update this book.
