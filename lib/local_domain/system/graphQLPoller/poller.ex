defmodule Anoma.LocalDomain.System.GraphQLPoller.Poller do
  @moduledoc """
  """

  use GenServer
  use Anoma.LocalDomain
  
  def start_link(opts), do: GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  
  
  def init(_opts) do
    state = %{
      cipher_keys: [],      
    }
    Process.send_after(self(), :tick, 0)

    {:ok, state}
  end

  def transactionExecutedQuery() do
  """
    query($min: Int!, $max: Int!) {
    raw_events(order_by: {block_number: asc}, where: {block_number: {_gt: $min, _lte: $max}}) {
    block_hash
    block_number
    block_fields
    event_name
    params
    }
    }
  """
  end

  def blockHeightQuery() do
  """
  query {
  raw_events(limit: 1, order_by: {block_number: asc}) {
  block_number
  }
  }
  """
  end
  
  def handle_info(:tick, state) do
    IO.puts("Polling now...")

    cipher_keys = 
      with {:ok, cipher_keys} <- Anoma.LocalDomain.Storage.ls(~k"/poller/cipherkey") do
        cipher_keys
        |> MapSet.to_list
        |> Enum.map(fn k ->
          with {:ok, v} <- Anoma.LocalDomain.Storage.read_latest(k) do
            v
          end
        end)
      end
    
    {:ok, endpoint} = Anoma.LocalDomain.Storage.read_latest(~k"/poller/graphql_endpoint")
    IO.puts(cipher_keys)
    IO.puts(endpoint)

    case Req.post(endpoint, json: %{query: blockHeightQuery()}) do
      {:ok, %{status: 200, body: body}} ->
        next_blockheight = Enum.at(body["data"]["raw_events"], 0)["block_number"]
        {:ok, current_blockheight} = Anoma.LocalDomain.System.GraphQLPoller.read_blockheight()
        case current_blockheight < next_blockheight do
          true ->
            case Req.post(endpoint, json: %{query: transactionExecutedQuery(), variables: %{"min" => current_blockheight, "max" => next_blockheight}}) do
              {:ok, %{status: 200, body: body}} ->
                IO.puts(body)

                for event <- body["data"]["raw_events"] do
                  if event["event_name"] == "TransactionExecuted" do
                    Anoma.LocalDomain.
                  end
                end
                
                Anoma.LocalDomain.System.GraphQLPoller.write_blockheight(next_blockheight)
              other -> IO.puts(other)
            end           
          false ->
            IO.puts(current_blockheight)
        end
        
      other -> IO.puts(other)
    end
    
    Process.send_after(self(), :tick, 5000) # every 5s
    {:noreply, state}
  end
  
end
