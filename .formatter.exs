# files that have to be formatted
inputs =
  [
    "*.{ex,exs}",
    "{apps,config,lib,test}/**/*.{ex,exs}"
  ]
  |> Enum.flat_map(&Path.wildcard(&1, match_dot: true))

# files that cannot be formatted
rejected =
  ["**/*.pb.ex"]
  |> Enum.flat_map(&Path.wildcard(&1, match_dot: true))

[
  inputs: inputs -- rejected,
  line_length: 78
]
