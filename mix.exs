defmodule Anoma.MixProject do
  use Mix.Project

  def version do
    {ver, _} = Code.eval_file("version.exs", ".")
    ver
  end

  def project do
    [
      apps_path: "apps",
      version: version(),
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
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
      # Docs
      name: "Anoma",
      docs: docs(),
      # Nockma eval
      escript: escript(),
      releases: releases(),
      package: package(),
      # these options are required for running `mix compile` in the umbrella.
      # make changes here and in apps/anoma_protobuf/mix.exs
      protoc_options: [
        elixir_out: "apps/anoma_protobuf/lib/anoma/protobuf",
        proto_files: ["apps/anoma_protobuf/priv/protobuf"],
        extra_opts:
          "one_file_per_module=true,gen_descriptors=true,plugins=grpc,include_docs=true"
      ]
    ]
  end

  def releases do
    [
      anoma_client: [
        include_executables_for: [:unix],
        applications: [
          {:anoma_client, :permanent}
        ]
      ],
      anoma: [
        include_executables_for: [:unix, :windows],
        applications: [
          anoma_node: :permanent,
          event_broker: :permanent
        ]
      ]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    []
  end

  defp docs do
    [
      main: "readme",
      extras: extras(),
      source_ref: "v#{version()}",
      javascript_config_path: "./.doc-versions.js",
      extra_section: "GUIDES",
      groups_for_extras: group_for_extras(),
      groups_for_modules: group_for_modules(),
      before_closing_body_tag: &docs_before_closing_body_tag/1,
      skip_undefined_reference_warnings_on:
        &(not String.match?(&1, ~r/^Protobuf\.Wire.*/))
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
      "Resource Machine": [
        ~r/^Anoma.RM.?/,
        ~r/^Anoma.Resource.?/,
        ~r/^Anoma.CairoResource.Resource.?/
      ],
      "Anoma Actors": [Anoma.Node],
      Mempool: ~r/^Anoma.Node.Mempool.?/,
      Executor: ~r/^Anoma.Node.Executor.?/,
      Identity: [~r/^Anoma.Identity.?/, ~r/^Anoma.Node.Identity.?/],
      Intents: ~r/^Anoma.Node.Intent.?/,
      Storage: [~r/^Anoma.Node.Storage.?/, Anoma.Node.Storage, Anoma.Order],
      Transport: ~r/^Anoma.Node.Transport.?/,
      Examples: ~r/^Examples.?/,
      "Cryptographic Primitives": [~r/^Anoma.Crypto.?/],
      Solver: [~r/^Anoma.Node.Solver.?/],
      "General Engines": [~r/^Anoma.Node.?/],
      "CLI Engine": [~r/^Anoma.Cli.?/],
      Nock: [~r/^Nock.?/, ~r/^Noun.?/],
      CommitmentTree: [~r/^CommitmentTree.?/],
      EventBroker: ~r/^EventBroker.?/,
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

  # Run "mix help compile.app" to learn about applications.
  # note: included_applications do *not* get started automatically
  #       extra_applications do get started automatically
  #       mnesia should *not* be started automatically
  def application do
    [
      extra_applications: [
        :observer,
        :wx
      ],
      included_applications: [:mnesia]
    ]
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
