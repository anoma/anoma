defmodule Anoma.Client.Web.IndexerController.Spec do
  alias OpenApiSpex.Schema

  require OpenApiSpex

  defmodule Nullifiers do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "List of nullifiers",
      type: :object,
      properties: %{
        nullifiers: %Schema{
          type: :list,
          description: "Base64 encoded, jammed representing a nullifier"
        }
      },
      required: [:nullifiers],
      example: %{
        "nullifiers" => [
          "TkZfAaHYNGTIOM4U"
        ]
      }
    })
  end

  defmodule UnrevealedCommits do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "List of unrevealed commits",
      type: :object,
      properties: %{
        commits: %Schema{
          type: :list,
          description:
            "Base64 encoded, jammed representing an unrevealed commit"
        }
      },
      required: [:commits],
      example: %{
        "commits" => [
          "Q01fAaHYNGTIOA6AAOG3T2NX290OA60mr6qwQHGWTvHJoPCr8Q7bBg4GyJCgKQ=="
        ]
      }
    })
  end

  defmodule Commits do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "List of unrevealed commits",
      type: :object,
      properties: %{
        commits: %Schema{
          type: :list,
          description: "Integer representing a commit"
        }
      },
      required: [:commits],
      example: %{
        "commits" => [
          "1537671851947758927876308911542417830990302212266205757362361328816119338469879118087480334645489341223095619"
        ]
      }
    })
  end

  defmodule UnspentResources do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "List of unspent resources",
      type: :object,
      properties: %{
        unspent_resources: %Schema{
          type: :list,
          description:
            "Base64 encoded, jammed representing an unspent resource"
        }
      },
      required: [:unspent_resources],
      example: %{
        "unspent_resources" => [
          "AaHYNGTIOA6AAOG3T2NX290OA60mr6qwQHGWTvHJoPCr8Q7bBg4GyJCgKQ=="
        ]
      }
    })
  end

  defmodule TransactionResult do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "A transaction in a block",
      type: :object,
      properties: %{
        result: %Schema{
          type: :string,
          description: "Success or error result"
        },
        value: %Schema{
          type: :list,
          description:
            "Base64 encoded, jammed noun representing the result of the transaction"
        }
      },
      required: [:result, :value],
      example: %{
        "result" => "success",
        "value" => "FfDWyvIq"
      }
    })
  end

  defmodule Transaction do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "A transaction in a block",
      type: :object,
      properties: %{
        code: %Schema{
          type: :string,
          description:
            "Base64 encoded, jammed representing the code of a transaction"
        },
        result: TransactionResult
      },
      required: [:code, :result],
      example: %{
        "code" => "BcFiiy0OooUsDnhrZXnDDMLFlthx40vjNBnU+Q==",
        "result" => %{"result" => "success", "value" => "FfDWyvKy"}
      }
    })
  end

  defmodule Block do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "A block",
      type: :object,
      properties: %{
        height: %Schema{
          type: :integer,
          description: "Height of the block"
        },
        transactions: %Schema{
          type: :array,
          items: Transaction
        }
      },
      required: [:height, :transactions],
      example: %{
        "unspent_resources" => [
          "AaHYNGTIOA6AAOG3T2NX290OA60mr6qwQHGWTvHJoPCr8Q7bBg4GyJCgKQ=="
        ]
      }
    })
  end

  defmodule Blocks do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "A list of blocks",
      type: :object,
      properties: %{
        blocks: %Schema{
          type: :array,
          items: Block
        }
      },
      required: [:blocks],
      example: %{
        "blocks" => []
      }
    })
  end

  defmodule Filter do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "A resource owner filter",
      type: :object,
      properties: %{
        owner: %Schema{
          type: :string
        },
        kind: %Schema{
          type: :string
        }
      },
      required: [:blocks],
      example: %{
        "owner" => "jeremy"
      }
    })
  end
end
