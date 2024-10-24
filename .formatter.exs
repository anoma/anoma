# Used by "mix format"
[
  inputs:
    ["*.{ex,exs}", "{config,lib,test}/**/*.{ex,exs}"] --
      Path.wildcard("**/*.pb.ex"),
  line_length: 78
]
