# keep me alphabetized; non-runtime dependencies at the bottom.
[
  {:anoma_lib,
   git: "https://github.com/anoma/anoma-lib",
   ref: "144da07055a76505e444c4359d63307e01671cfb", override: true},
  {:anoma_protobuf,
   git: "https://github.com/anoma/anoma-protobuf", tag: "v1.0.0"},
  {:arm_openvm,
   git: "https://github.com/anoma/arm-openvm",
   ref: "e264d9004231072365663da51c54d262333a8914"},
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
