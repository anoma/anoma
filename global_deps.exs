# keep me alphabetized; non-runtime dependencies at the bottom.
[
  {:burrito, "~> 1.1.0"},
  {:cairo,
    git: "https://github.com/anoma/aarm-cairo",
    rev: "dd2ed877e8d7c326a4a88dcf86ae32cd0183f3ae"},
  {:enacl, git: "https://github.com/anoma/enacl/"},
  {:kino, git: "https://github.com/livebook-dev/kino", override: true},
  # until the next Kino release
  {:kino_kroki, "~> 0.1.0"},
  {:memoize, "~> 1.4.3"},
  {:mnesia_rocksdb, git: "https://github.com/mariari/mnesia_rocksdb"},
  {:msgpack, "~> 0.8.1"},
  {:murmur, "~> 2.0"},
  {:optimus, "~> 0.2"},
  {:plug_crypto, "~> 2.0"},
  {:qex, ">= 0.5.1"},
  {:recon, "~> 2.5.4"},
  {:rexbug, ">= 2.0.0-rc1"},
  {:toml, "~> 0.7"},
  {:typed_struct, "~> 0.3.0"},
  # non-runtime dependencies below
  {:dialyxir, "~> 1.3", only: [:dev], runtime: false},
  {:ex_doc, "~> 0.31", only: [:dev], runtime: false},
]
