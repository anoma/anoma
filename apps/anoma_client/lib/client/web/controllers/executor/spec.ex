defmodule Anoma.Client.Web.ExecutorController.Spec do
  alias OpenApiSpex.Schema

  require OpenApiSpex

  defmodule ScryKey do
    @example_key ["anoma", "blob", "key"]
                 |> Noun.Jam.jam()
                 |> Base.encode64()

    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "A Base64 encoded representation of a scry key",
      type: :object,
      properties: %{
        key: %Schema{
          type: :string,
          description: "Base64 encoded, jammed scry key"
        }
      },
      required: [:program],
      example: %{
        "key" => @example_key
      }
    })
  end

  defmodule Result do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "Result of running a scry",
      type: :object,
      properties: %{
        value: %Schema{
          type: :string,
          description: "Base64 encoded, jammed result of running a scry"
        }
      },
      required: [:program],
      example: %{
        "result" => "AXzSQMLaQMJA5sroKQ=="
      }
    })
  end
end
