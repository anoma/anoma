defmodule Anoma.MixProject do
  use Mix.Project

  def project do
    [
      app: :anoma,
      version: "0.5.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: [
        plt_local_path: "plts/anoma.plt",
        plt_core_path: "plts/core.plt",
        flags: ["-Wno_improper_lists"]
      ],
      # Docs
      name: "Anoma",
      docs: docs(),
      # Nockma eval
      escript: [
        main_module: Nock.Cli,
        name: "nockma",
        app: nil
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {Anoma, []},
      extra_applications: [
        :logger,
        :crypto,
        :mnesia,
        :observer,
        :wx,
        :runtime_tools,
        :debugger
      ]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:mnesia_rocksdb, git: "https://github.com/mariari/mnesia_rocksdb"},
      {:typed_struct, "~> 0.3.0"},
      {:xxhash, "~> 0.3"},
      {:recon, "~> 2.5.4"},
      {:rexbug, ">= 2.0.0-rc1"},
      {:kino, "~> 0.12.2"},
      {:ex_doc, "~> 0.31", only: [:dev], runtime: false},
      {:dialyxir, "~> 1.3", only: [:dev], runtime: false},
      {:optimus, "~> 0.2"}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: extras(),
      extra_section: "GUIDES",
      groups_for_extras: group_for_extras(),
      groups_for_modules: group_for_modules()
    ]
  end

  def group_for_modules() do
    [
      "Resource Machine": [~r/^Nock.?/, ~r/^Noun.?/],
      "Anoma Actors": [Anoma.Node],
      Mempool: ~r/^Anoma.Node.Mempool.?/,
      Executor: ~r/^Anoma.Node.Executor.?/,
      Intents: ~r/^Anoma.Node.Intent.?/,
      Storage: [~r/^Anoma.Node.Storage.?/, Anoma.Storage, Anoma.Order],
      Utilities: [Anoma.Node.Utility, Anoma.Mnesia],
      "Test Helpers": ~r/^TestHelper.?/,
      "Deprecated Logic": [~r/^Anoma.Logic/, Anoma.Eval]
    ]
  end

  def group_for_extras() do
    [
      "Contributors Guide": ~r/documentation\/contributing\/.?/,
      "Contributors Guide": "documentation/CONTRIBUTING.livemd",
      "Visualizing Anoma": ~r/documentation\/visualization\/.?/,
      "Visualizing Anoma": "documentation/visualization.livemd"
    ]
  end

  def extras() do
    [
      "README.md",
      "documentation/index.livemd",
      "documentation/CONTRIBUTING.livemd",
      "documentation/contributing/iex.livemd",
      "documentation/contributing/observer.livemd",
      "documentation/contributing/understanding.livemd",
      "documentation/contributing/testing.livemd",
      "documentation/contributing/git.livemd",
      "documentation/visualization.livemd",
      "documentation/visualization/actors.livemd"
    ]
  end
end
