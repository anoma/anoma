defmodule Anoma.Client.Web.SubscribeController.Spec do
  alias OpenApiSpex.Schema

  require OpenApiSpex

  defmodule SubscribeRequest do
    OpenApiSpex.schema(%{
      description: "Subscribe to events from the node",
      type: :object,
      properties: %{
        topic: %Schema{
          type: :string,
          default: "*",
          description: "The topic to subscribe to"
        }
      },
      required: [:tppic],
      example: %{
        "topic" => "*"
      }
    })
  end

  defmodule SubscribeResult do
    OpenApiSpex.schema(%{
      description: "Result message of adding the transaction",
      type: :object,
      properties: %{
        message: %Schema{
          type: :string,
          default: "subscribed",
          description: "The result message"
        }
      },
      required: [:message],
      example: %{
        "message" => "subscribed"
      }
    })
  end
end
