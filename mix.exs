defmodule Anoma.MixProject do
  use Mix.Project

  def project do
    [
      app: :anoma,
      version: "0.2.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: [
        plt_local_path: "plts/anoma.plt",
        plt_core_path: "plts/core.plt",
        flags: ["-Wno_improper_lists"]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :crypto, :mnesia]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:mnesia_rocksdb, git: "https://github.com/mariari/mnesia_rocksdb"},
      {:typed_struct, "~> 0.3.0"},
      {:xxhash, "~> 0.3"},
      {:dialyxir, "~> 1.3", only: [:dev], runtime: false}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
