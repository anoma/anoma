defmodule Anoma.Client.Web.IndexerJSON do
  def render("binaries.json", assigns) do
    for {k, v} <- Map.delete(assigns, :conn) do
      {k, Enum.map(v, &Base.encode64/1)}
    end
    |> Enum.into(%{})
  end

  def render("commits.json", %{commits: commits}) do
    %{commits: commits}
  end

  def render("unspent_resources.json", %{unspent_resources: unspent_resources}) do
    %{unspent_resources: unspent_resources}
  end

  def render("transaction.json", %{transaction: transaction}) do
    {atom, result} = transaction.result

    %{
      code: Base.encode64(transaction.code),
      result: %{result: atom, value: Base.encode64(result)}
    }
  end

  def render("block.json", %{block: nil}) do
    %{block: nil}
  end

  def render("block.json", %{block: block}) do
    %{
      block: %{
        transactions:
          Enum.map(
            block.transactions,
            &render("transaction.json", %{transaction: &1})
          ),
        height: block.height
      }
    }
  end

  def render("blocks.json", %{blocks: blocks}) do
    blocks = Enum.map(blocks, &render("block.json", %{block: &1}))
    %{blocks: blocks}
  end

  def render("root.json", %{root: root}) do
    %{root: root}
  end
end
