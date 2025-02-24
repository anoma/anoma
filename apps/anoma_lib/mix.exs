defmodule AnomaLib.MixProject do
  use Mix.Project

  def version do
    {ver, _} = Code.eval_file("version.exs", "../..")
    ver
  end

  def project do
    [
      app: :anoma_lib,
      version: version(),
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      test_coverage: [
        ignore_modules: [NockPoly.Term.Unreachable]
      ]
    ]
  end

  # note: included_applications do *not* get started automatically
  #       extra_applications do get started automatically
  #       mnesia should *not* be started automatically
  def application do
    [
      included_applications: [:mnesia]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:anoma_protobuf, in_umbrella: true}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},
      # {:sibling_app_in_umbrella, in_umbrella: true}
    ] ++ global_deps()
  end

  def global_deps do
    {list, _} = Code.eval_file("global_deps.exs", "../..")
    list
  end
end
