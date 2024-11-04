defmodule Mix.Tasks.Toc do
  @moduledoc "I generate out the TOC for each liveview doc"

  @shortdoc "Simply adds a TOC to each liveview doc"
  use Mix.Task

  @impl true
  def run(_args) do
    Livebook.toc_toplevel()
  end
end
