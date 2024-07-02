defmodule Examples.EConfiguration do
  alias Anoma.Configuration

  @spec dumper_config(String.t()) :: Configuration.configuration_map()
  def dumper_config(name) do
    Configuration.configuration(%{
      "node" => %{"block_storage" => name},
      "dump" => %{"dump" => "./test/dumper.dmp"}
    })
  end
end
