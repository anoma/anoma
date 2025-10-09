defmodule Anoma.Client.Web.MempoolController.Spec do
  alias OpenApiSpex.Schema

  require OpenApiSpex

  defmodule AddTransaction do
    # This is the result of
    # `Anoma.Client.Examples.EClient.Intents.example_intent()`, but it cannot be
    # executed at compiletime due to the memoization.
    @example_intent "BcDBAIg21PQVEIpNQ4aM4wDkqw6z4fYP7WK+SFa1lkR/z5v29wgjydb+6psttDpLMHqappUFQDQno6+AUGwaMmQcByBfdZgNt39oF/NFsqq1JPp73rS/RxhJtvZX32yh1VmC0VOrYVgHhGJT08qqYVg3cyiiTQCGFID/FuPxlBs2TyS5cgFnbXQgSHKk/RMz4TV9i8zKyzu8rCsyNTUV"

    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "Adding a transaction to the mempool",
      type: :object,
      properties: %{
        transaction: %Schema{
          type: :string,
          description:
            "Base64 encoded, jammed nock representing a transaction"
        },
        wrap: %Schema{
          type: :bool,
          description:
            "If this flag is set, it will wrap the transaction in a transaction candidate before submitting it to the mempool."
        },
        transaction_type: %Schema{
          type: :string,
          description: "The type of the transaction",
          default: "transparent_resource",
          enum: ["transparent_resource", "cairo"]
        }
      },
      required: [:transaction],
      example: %{
        "transaction" => @example_intent,
        "transaction_type" => "transparent_resource",
        "wrap" => false
      }
    })
  end

  defmodule AddResult do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "Result of adding a transaction to the mempool",
      type: :object,
      properties: %{
        message: %Schema{
          type: :string,
          default: "transaction added",
          description: "The result message"
        }
      },
      required: [:message],
      example: %{
        "message" => "transaction added"
      }
    })
  end
end
