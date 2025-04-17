defmodule Anoma.Client.Web.ExecutorController.Spec do
  alias OpenApiSpex.Schema

  require OpenApiSpex

  defmodule ReadOnlyTransaction do
    @example_tx Anoma.Node.Examples.EExecutor.read_only_transaction()
                |> Noun.Jam.jam()
                |> Base.encode64()

    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description:
        "A Base64 encoded representation of a read-only transaction",
      type: :object,
      properties: %{
        transaction: %Schema{
          type: :string,
          description: "Base64 encoded, jammed transaction candidate"
        }
      },
      required: [:program],
      example: %{
        "transaction" => @example_tx
      }
    })
  end

  defmodule Result do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "Result of submitting the transaction",
      type: :object,
      properties: %{
        transaction: %Schema{
          type: :string,
          description:
            "Base64 encoded, jammed result of read-only transaction"
        }
      },
      required: [:program],
      example: %{
        "result" => "FfDWyvIq"
      }
    })
  end
end
