import_file_if_available("~/.iex.exs")

defmodule Dev do
  defmacro __using__(_) do
    if Mix.env() == :dev do
      quote do
        alias Anoma.TransparentResource.{
          Action,
          Delta,
          LogicProof,
          Resource,
          Transaction
        }

        alias Anoma.Node.Intents.{IntentPool, Solver}
        alias Anoma.Node.Logging

        alias Anoma.Node.Transaction.{
          Backends,
          Executor,
          Mempool,
          Ordering,
          Storage
        }

        alias Nock.{Bits, Cue, Jam, Jets}
      end
    end
  end
end

use Dev
