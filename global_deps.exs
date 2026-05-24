# keep me alphabetized; non-runtime dependencies at the bottom.
[
  {:anoma_lib,
   git: "https://github.com/anoma/anoma-lib",
   ref: "0a489c227334249aaf557a590ee03464181e7fac"},
  {:anoma_protobuf,
   git: "https://github.com/anoma/anoma-protobuf", tag: "v1.0.0"},
  {:event_broker,
   git: "https://github.com/anoma/event-broker", tag: "v1.0.0"},
  {:ex_keccak, "~> 0.7.6"},
  {:grpc, "~> 0.9"},
  {:jason, "~> 1.4"},
  {:protobuf, "~> 0.11.0"},
  {:typed_struct, "~> 0.3.0"},
  # non-runtime dependencies below
  {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
  {:dialyxir, "~> 1.4", only: [:dev], runtime: false},
  {:ex_doc, "~> 0.31", only: [:dev], runtime: false}
]
