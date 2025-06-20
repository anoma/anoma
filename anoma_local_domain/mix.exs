defmodule AnomaLocalDomain.MixProject do
  use Mix.Project

  def project do
    [
      app: :anoma_local_domain,
      version: "0.1.0",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      included_applications: [:anoma]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:anoma, git: "https://github.com/anoma/anoma", tag: "v0.34.0"}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
