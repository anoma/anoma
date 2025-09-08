defmodule AnomaIntentLab.MixProject do
  use Mix.Project

  def project do
    [
      app: :anoma_intent_lab,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:stream_data, "~> 0.6", only: :test}
    ]
  end
end
