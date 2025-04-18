defmodule Anoma.Client.Web.IndexerController.Spec do
  alias OpenApiSpex.Schema

  require OpenApiSpex

  defmodule Nullifier do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "Base64 encoded, jammed representing a nullifier",
      type: :string,
      example: "TkZfAaHYNGTIOM4U"
    })
  end

  defmodule Nullifiers do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "List of nullifiers",
      type: :object,
      properties: %{
        nullifiers: %Schema{
          type: :array,
          items: Nullifier
        }
      },
      required: [:nullifiers],
      example: %{
        "nullifiers" => [
          Nullifier.schema().example,
          Nullifier.schema().example
        ]
      }
    })
  end

  defmodule UnrevealedCommit do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "Base64 encoded, jammed representing an unrevealed commit",
      type: :string,
      example:
        "Q01fAaHYNGTIOA6AAOG3T2NX290OA60mr6qwQHGWTvHJoPCr8Q7bBg4GyJCgKQ=="
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
          type: :array,
          items: UnrevealedCommit
        }
      },
      required: [:commits],
      example: %{
        "commits" => [
          UnrevealedCommit.schema().example,
          UnrevealedCommit.schema().example
        ]
      }
    })
  end

  defmodule Commit do
    OpenApiSpex.schema(%{
      description: "Integer representing a commit",
      type: :string,
      example:
        "1537671851947758927876308911542417830990302212266205757362361328816119338469879118087480334645489341223095619"
    })
  end

  defmodule Commits do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "List of commits",
      type: :object,
      properties: %{
        commits: %Schema{
          type: :array,
          items: Commit
        }
      },
      required: [:commits],
      example: %{
        "commits" => [
          Commit.schema().example,
          Commit.schema().example
        ]
      }
    })
  end

  defmodule UnspentResource do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "Base64 encoded, jammed representing an unspent resource",
      type: :string,
      example: "AaHYNGTIOA6AAOG3T2NX290OA60mr6qwQHGWTvHJoPCr8Q7bBg4GyJCgKQ=="
    })
  end

  defmodule UnspentResources do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "List of unspent resources",
      type: :object,
      properties: %{
        commits: %Schema{
          type: :array,
          items: UnspentResource
        }
      },
      required: [:commits],
      example: %{
        "unspent_resources" => [
          UnspentResource.schema().example,
          UnspentResource.schema().example
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
        "blocks" => [Block.schema().example]
      }
    })
  end

  defmodule Filter do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "A resource filter",
      type: :object,
      oneOf: [
        %Schema{
          description: "Kind filter",
          type: :object,
          properties: %{
            kind: %Schema{
              type: :string,
              description: "The kind of the resoruce, base64 encoded."
            }
          }
        },
        %Schema{
          description: "Owner filter",
          type: :object,
          properties: %{
            kind: %Schema{
              type: :string,
              description:
                "The owner of the resource, left-padded to 32 bytes, base64 encoded"
            }
          }
        }
      ],
      example: %{
        "owner" => "amVyZW15AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
      }
    })
  end

  defmodule Filters do
    OpenApiSpex.schema(%{
      # The title is optional. It defaults to the last section of the module name.
      # So the derived title for MyApp.User is "User".
      description: "A list of resource filters",
      type: :object,
      properties: %{
        filters: %Schema{
          type: :array,
          items: Filter
        }
      },
      example: %{
        "filters" => [
          %{
            "owner" => "amVyZW15AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
          },
          %{"kind" => "LcbjKTdsnkhy5QLO2uhAkORI+ydmwmv6FpmVl3d4WVc="}
        ]
      }
    })
  end
end
