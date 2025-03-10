# keep me alphabetized; non-runtime dependencies at the bottom.
[
  {:cairo,
   git: "https://github.com/anoma/aarm-cairo",
   ref: "a5a2778a4ad9b2ff40cea471ac777089149b9fda"},
  {:enacl,
   git: "https://github.com/anoma/enacl/",
   ref: "23173637c495b85d56f205e4721cfe5afdef92e9"},
  {:kino, git: "https://github.com/livebook-dev/kino", override: true},
  # until the next Kino release
  {:grpc_reflection, "~> 0.1.0"},
  {:kino_kroki, "~> 0.1.0"},
  {:memoize, "~> 1.4.3"},
  {:mnesia_rocksdb, git: "https://github.com/aeternity/mnesia_rocksdb"},
  {:mock, "~> 0.3.0"},
  {:msgpack, "~> 0.8.1"},
  {:murmur, "~> 2.0"},
  {:optimus, "~> 0.2"},
  {:plug_crypto, "~> 2.0"},
  {:qex, ">= 0.5.1"},
  {:recon, "~> 2.5.4"},
  {:rexbug, ">= 2.0.0-rc1"},
  {:toml, "~> 0.7"},
  {:typed_struct, "~> 0.3.0"},
  {:lexical_credo, "~> 0.1.0", only: [:dev, :test]},
  # non-runtime dependencies below
  {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
  {:dialyxir, "~> 1.4", only: [:dev], runtime: false},
  {:ex_doc, "~> 0.31", only: [:dev], runtime: false}
]
