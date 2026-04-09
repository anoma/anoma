defmodule AnomaNode.MixProject do
  use Mix.Project

  def version do
    {ver, _} = Code.eval_file("version.exs")
    ver
  end

  def project do
    [
      app: :anoma_node,
      version: version(),
      build_path: "_build",
      config_path: "config/config.exs",
      dialyzer: [
        plt_local_path: "plts/anoma.plt",
        plt_core_path: "plts/core.plt",
        flags: [
          # Turn off the warning for improper lists, because we use
          # bare cons frequently and deliberately.
          "-Wno_improper_lists"
        ],
        plt_add_apps: [:mix, :ex_unit, :mnesia]
      ],
      deps_path: "deps",
      lockfile: "mix.lock",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      package: package(),
      deps: deps()
    ]
  end

  def package do
    [
      maintainers: ["Mariari", " Raymond E. Pasco"],
      name: :anoma_node,
      licenses: ["MIT"]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  # note: included_applications do *not* get started automatically
  #       extra_applications do get started automatically
  #       mnesia should *not* be started automatically
  def application do
    [
      mod: {Anoma.Node, []},
      extra_applications: [
        :crypto,
        :debugger,
        :enacl,
        :logger,
        :runtime_tools,
        :tools,
        :ex_unit
      ],
      included_applications: [:mnesia]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [] ++ global_deps()
  end

  def global_deps do
    {list, _} = Code.eval_file("global_deps.exs")
    list
  end
end
