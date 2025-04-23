defmodule Anoma.Client.MixProject do
  use Mix.Project

  def project do
    [
      app: :anoma_client,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: [main_module: Anoma.Client.CLI],
      runtime: false,
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib", "test/support"]

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Anoma.Client.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:anoma_node, in_umbrella: true, runtime: false},
      {:anoma_lib, in_umbrella: true},
      {:anoma_protobuf, in_umbrella: true},
      {:protobuf, "~> 0.11.0"},
      {:grpc, "~> 0.9"},
      {:phoenix, "~> 1.7.14"},
      {:bandit, "~> 1.5"},
      {:open_api_spex, "~> 3.21"}
    ]
  end
end
