defmodule Anoma.LocalDomain.MixProject do
  use Mix.Project

  def project do
    [
      app: :anoma_local_domain,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :observer, :wx],
      included_applications: [:anoma],
      mod: {Anoma.LocalDomain.OTPApplication, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # library dependencies here
      {:anoma, git: "https://github.com/anoma/anoma", tag: "v0.34.0"},
      {:req, "~> 0.5.0"},
      {:typed_struct, "~> 0.3.0"},
      # non-runtime dependencies here
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.4", only: :dev, runtime: false},
      {:ex_doc, "~> 0.38.2", only: :dev, runtime: false}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
