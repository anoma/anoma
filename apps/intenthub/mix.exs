defmodule IntentHub.MixProject do
  use Mix.Project

  def project do
    [
      app: :intenthub,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {IntentHub.Application, []}
    ]
  end

  defp deps do
    [
      {:anoma_lib, in_umbrella: true}
    ]
  end
end
