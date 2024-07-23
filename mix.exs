defmodule Anoma.MixProject do
  use Mix.Project

  @version "0.18.0"

  def project do
    [
      app: :anoma,
      version: @version,
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: [
        plt_local_path: "plts/anoma.plt",
        plt_core_path: "plts/core.plt",
        flags: ["-Wno_improper_lists"],
        plt_add_apps: [:mix, :ex_unit]
      ],
      # Docs
      name: "Anoma",
      docs: docs(),
      # Nockma eval
      escript: escript(),
      # Burrito release
      releases: releases(),
      package: package()
    ]
  end

  def releases do
    [
      anoma: [
        steps: [:assemble, &Burrito.wrap/1],
        burrito: [
          targets: [
            linux: [os: :linux, cpu: :x86_64]
            # macos: [os: :darwin, cpu: :x86_64],
            # macos_m1: [os: :darwin, cpu: :aarch64],
            # windows: [os: :windows, cpu: :x86_64]
          ]
        ]
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
        :debugger,
        :enacl,
        :tools
      ]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:enacl, git: "https://github.com/anoma/enacl/"},
      {:mnesia_rocksdb, git: "https://github.com/mariari/mnesia_rocksdb"},
      {:typed_struct, "~> 0.3.0"},
      {:xxhash,
       git: "https://github.com/htdat148/erlang-xxhash-otp-26",
       branch: "fix_erlang_get_path_otp26"},
      {:recon, "~> 2.5.4"},
      {:rexbug, ">= 2.0.0-rc1"},
      # until the next Kino release
      {:kino_kroki, "~> 0.1.0"},
      {:kino, git: "https://github.com/livebook-dev/kino", override: true},
      {:ex_doc, "~> 0.31", only: [:dev], runtime: false},
      {:dialyxir, "~> 1.3", only: [:dev], runtime: false},
      {:optimus, "~> 0.2"},
      {:burrito, "~> 1.0.4"},
      {:toml, "~> 0.7"},
      {:cairo,
       git: "https://github.com/anoma/aarm-cairo",
       rev: "961910bfb799e25a10a8ad16c4e8e07015ba1858"},
      {:plug_crypto, "~> 2.0"},
      {:memoize, "~> 1.4.3"},
      {:msgpack, "~> 0.8.1"}
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: extras(),
      source_ref: "v#{@version}",
      javascript_config_path: "./.doc-versions.js",
      extra_section: "GUIDES",
      groups_for_extras: group_for_extras(),
      groups_for_modules: group_for_modules(),
      before_closing_body_tag: &docs_before_closing_body_tag/1
    ]
  end

  def package do
    [
      maintainers: ["Mariari", " Raymond E. Pasco"],
      name: :anoma,
      licenses: ["MIT"]
    ]
  end

  def group_for_modules() do
    [
      "Resource Machine": [~r/^Nock.?/, ~r/^Noun.?/, ~r/^Anoma.Resource.?/],
      "Anoma Actors": [Anoma.Node],
      Mempool: ~r/^Anoma.Node.Mempool.?/,
      Executor: ~r/^Anoma.Node.Executor.?/,
      Identity: [~r/^Anoma.Identity.?/, ~r/^Anoma.Node.Identity.?/],
      Intents: ~r/^Anoma.Node.Intent.?/,
      Storage: [~r/^Anoma.Node.Storage.?/, Anoma.Node.Storage, Anoma.Order],
      "Cryptographic Primitives": [~r/^Anoma.Crypto.?/],
      Solver: [~r/^Anoma.Node.Solver.?/],
      CommitmentTree: [~r/^CommitmentTree.?/],
      Utilities: [Anoma.Utility, Anoma.Mnesia, Anoma.Communicator],
      "Test Helpers": ~r/^TestHelper.?/
    ]
  end

  def group_for_extras() do
    [
      "Guide Index": "documentation/index_docs.livemd",
      "Contributors Guide": ~r/documentation\/contributing\/.?/,
      "Contributors Guide": "documentation/CONTRIBUTING.livemd",
      "Visualizing Anoma": ~r/documentation\/visualization\/.?/,
      "Visualizing Anoma": "documentation/visualization.livemd",
      "Nock Environment": ~r/documentation\/hoon\/.?/,
      "Nock Environment": "documentation/hoon.livemd"
    ]
  end

  def extras() do
    ["README.md" | all_docs("./documentation")]
  end

  def escript do
    [
      main_module: Anoma.Cli,
      name: "anoma",
      app: nil
    ]
  end

  defp docs_before_closing_body_tag(:html) do
    # https://hexdocs.pm/ex_doc/readme.html#extensions
    """
    <script src="https://cdn.jsdelivr.net/npm/mermaid@10.7.0/dist/mermaid.min.js"></script>
    <script>
    document.addEventListener("DOMContentLoaded", function () {
    mermaid.initialize({
      startOnLoad: false,
      theme: document.body.className.includes("dark") ? "dark" : "default"
    });
    let id = 0;
    for (const codeEl of document.querySelectorAll("pre code.mermaid")) {
      const preEl = codeEl.parentElement;
      const graphDefinition = codeEl.textContent;
      const graphEl = document.createElement("div");
      const graphId = "mermaid-graph-" + id++;
      mermaid.render(graphId, graphDefinition).then(({svg, bindFunctions}) => {
        graphEl.innerHTML = svg;
        bindFunctions?.(graphEl);
        preEl.insertAdjacentElement("afterend", graphEl);
        preEl.remove();
      });
    }
    });
    </script>
    <script src="https://cdn.jsdelivr.net/npm/vega@5.20.2"></script>
    <script src="https://cdn.jsdelivr.net/npm/vega-lite@5.1.1"></script>
    <script src="https://cdn.jsdelivr.net/npm/vega-embed@6.18.2"></script>
    <script>
    document.addEventListener("DOMContentLoaded", function () {
    for (const codeEl of document.querySelectorAll("pre code.vega-lite")) {
      try {
        const preEl = codeEl.parentElement;
        const spec = JSON.parse(codeEl.textContent);
        const plotEl = document.createElement("div");
        preEl.insertAdjacentElement("afterend", plotEl);
        vegaEmbed(plotEl, spec);
        preEl.remove();
      } catch (error) {
        console.log("Failed to render Vega-Lite plot: " + error)
      }
    }
    });
    </script>
    """
  end

  defp docs_before_closing_body_tag(_), do: ""

  defp all_docs(dir) do
    [dir | dir_from_path(dir)]
    |> Stream.map(fn x -> Path.wildcard(Path.join(x, "*livemd")) end)
    |> Stream.concat()
    |> Enum.sort()
  end

  defp dir_from_path(dir) do
    File.ls!(dir)
    |> Stream.map(&Path.join(dir, &1))
    |> Stream.filter(&File.dir?(&1))
    |> Enum.map(fn x -> [x | dir_from_path(x)] end)
    |> Enum.concat()
  end
end
