defmodule Anoma.Protobuf.MixProject do
  use Mix.Project

  def version do
    {ver, _} = Code.eval_file("version.exs", "../..")
    ver
  end

  def project do
    [
      app: :anoma_protobuf,
      version: version(),
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      compilers: [:protoc] ++ Mix.compilers(),
      # these options are required for running `mix deps.compile` in the umbrella.
      # make changes here and in ../../mix.exs.
      protoc_options: [
        elixir_out: "lib/anoma/protobuf",
        proto_files: ["priv/protobuf"],
        extra_opts:
          "one_file_per_module=true,gen_descriptors=true,plugins=grpc"
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:anoma_lib, in_umbrella: true},
      {:compile_protoc, in_umbrella: true},
      {:grpc, "~> 0.9"},
      {:protobuf, "~> 0.11.0"}
    ] ++ global_deps()
  end

  def global_deps do
    {list, _} = Code.eval_file("global_deps.exs", "../..")
    list
  end
end
