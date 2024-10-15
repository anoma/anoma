# Used by "mix format"
[
  inputs:
    ["*.{ex,exs}", "{apps,config,lib,test}/**/*.{ex,exs}"]
    |> Enum.flat_map(&Path.wildcard(&1, match_dot: true))
    |> Enum.reject(&(&1 =~ ~r/^.*.pb.ex/)),
  line_length: 78
]
