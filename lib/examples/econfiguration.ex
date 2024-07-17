defmodule Examples.EConfiguration do
  alias Anoma.Configuration

  @spec dumper_config(String.t()) :: Configuration.configuration_map()
  def dumper_config(name) do
    Configuration.configuration(%{
      "node" => %{
        "block_storage" => name <> "_blocks",
        "name" => name,
        "order" => name <> ".Order",
        "qualified" => name <> ".Qualified"
      },
      "dump" => %{"dump" => "./test/dumper.dmp"}
    })
  end
end
