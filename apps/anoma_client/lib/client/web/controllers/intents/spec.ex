defmodule Anoma.Client.Web.IntentsController.Spec do
  alias OpenApiSpex.Schema

  require OpenApiSpex

  defmodule IntentList do
    # This is the result of
    # `Anoma.Client.Examples.EClient.Intents.example_intent()`, but it cannot be
    # executed at compiletime due to the memoization.
    @example_intent "BcDBAIg21PQVEIpNQ4aM4wDkqw6z4fYP7WK+SFa1lkR/z5v29wgjydb+6psttDpLMHqappUFQDQno6+AUGwaMmQcByBfdZgNt39oF/NFsqq1JPp73rS/RxhJtvZX32yh1VmC0VOrYVgHhGJT08qqYVg3cyiiTQCGFID/FuPxlBs2TyS5cgFnbXQgSHKk/RMz4TV9i8zKyzu8rCsyNTUV"

    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "List of intents currently in the intentpool",
      type: :object,
      properties: %{
        intents: %Schema{
          type: :list,
          description: "Base64 encoded, jammed representing an intent"
        }
      },
      required: [:program],
      example: %{
        "intents" => [
          @example_intent
        ]
      }
    })
  end

  defmodule AddResult do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "Result of submitting an intent",
      type: :object,
      properties: %{
        message: %Schema{
          type: :string,
          default: "intent added",
          description: "The result message"
        }
      },
      required: [:message],
      example: %{
        "message" => "intent added"
      }
    })
  end

  defmodule AddIntent do
    # This is the result of
    # `Anoma.Client.Examples.EClient.Intents.example_intent()`, but it cannot be
    # executed at compiletime due to the memoization.
    @example_intent "BcDBAIg21PQVEIpNQ4aM4wDkqw6z4fYP7WK+SFa1lkR/z5v29wgjydb+6psttDpLMHqappUFQDQno6+AUGwaMmQcByBfdZgNt39oF/NFsqq1JPp73rS/RxhJtvZX32yh1VmC0VOrYVgHhGJT08qqYVg3cyiiTQCGFID/FuPxlBs2TyS5cgFnbXQgSHKk/RMz4TV9i8zKyzu8rCsyNTUV"

    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "an ",
      type: :object,
      properties: %{
        intent: %Schema{
          type: :string,
          description: "Base64 encoded, jammed representing an intent"
        }
      },
      required: [:intent],
      example: %{
        "intent" => @example_intent
      }
    })
  end
end
