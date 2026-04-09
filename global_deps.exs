# keep me alphabetized; non-runtime dependencies at the bottom.
[
  {:anoma_lib, git: "https://github.com/anoma/anoma-lib", tag: "v1.0.1"},
  {:anoma_protobuf,
   git: "https://github.com/anoma/anoma-protobuf", tag: "v1.0.0"},
  {:event_broker, git: "https://github.com/anoma/event-broker", tag: "v1.0.0"},
  {:grpc, "~> 0.9"},
  {:jason, "~> 1.4"},
  {:protobuf, "~> 0.11.0"},
  {:typed_struct, "~> 0.3.0"},
  # non-runtime dependencies below
  {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
  {:dialyxir, "~> 1.4", only: [:dev], runtime: false},
  {:ex_doc, "~> 0.31", only: [:dev], runtime: false}
]
