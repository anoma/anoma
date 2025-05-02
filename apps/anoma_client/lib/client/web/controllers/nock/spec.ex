defmodule Anoma.Client.Web.NockController.Spec do
  alias OpenApiSpex.Schema
  alias Anoma.Client.Examples.EClient.Nock

  require OpenApiSpex

  defmodule ProveRequest do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description:
        "A Base64 encoded representation of a Nock program to prove",
      type: :object,
      properties: %{
        program: %Schema{
          type: :string,
          description: "Base64 encoded, jammed Nock"
        },
        public_inputs: %Schema{
          type: :list,
          description: "Base64 encoded, jammed Nock inputs"
        },
        private_inputs: %Schema{
          type: :list,
          description: "Base64 encoded, jammed Nock inputs"
        }
      },
      required: [:program],
      example: %{
        "program" => Base.encode64(Nock.squared()),
        "public_inputs" => [Base.encode64(Noun.Jam.jam(3))],
        "private_inputs" => []
      }
    })
  end

  defmodule RunRequest do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "A Base64 encoded representation of a Nock program to run",
      type: :object,
      properties: %{
        program: %Schema{
          type: :string,
          description: "Base64 encoded, jammed Nock"
        },
        inputs: %Schema{
          type: :list,
          description: "Base64 encoded, jammed Nock inputs"
        }
      },
      required: [:program],
      example: %{
        "program" => Base.encode64(Nock.tracing()),
        "inputs" => ["aA=="]
      }
    })
  end

  defmodule Result do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      title: "Result of Nock evaluation",
      description: "A Base64 encoded representation of a Nock program.",
      type: :object,
      properties: %{
        result: %Schema{
          type: :string,
          description: "Base64 encoded, jammed Nock"
        },
        io: %Schema{
          type: :list,
          description: "List of IO hints during execution"
        }
      },
      example: %{
        "result" => "deadbeefd",
        "io" => ["deadbeef"]
      }
    })
  end
end
