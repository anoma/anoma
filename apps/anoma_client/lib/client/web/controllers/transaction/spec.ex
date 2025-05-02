defmodule Anoma.Client.Web.TransactionController.Spec do
  alias OpenApiSpex.Schema

  require OpenApiSpex

  defmodule ComposeTransactions do
    # the intent is the result of
    #
    # Examples.ETransparent.ETransaction.swap_from_actions_non_eph_nullifier |>
    # Noun.Nounable.to_noun  |> Noun.Jam.jam() |> Base.encode64
    #
    # but it cannot be executed at compiletime due to the memoization.
    @example_tx "BcDCAKg21PQVEIpNQ4aM4wD0D004KZaOlGaR53uZBnU+j47ngkJRvKGUEl6KkLOYhvppmlYBUG2o6SsgFJuGDBnHAegPJpL9EQMk1OCEfg58Iy2rydpJHtHwgrMLIU4Ceg5lIVOzGkZ1QCg2Na0sq2FUD4W0APQoAP8txuMpN2yeSHLlAs7a6ECQ5Ej7J2bCa/oWmZWXd3hZV05NrSwAqjkZfQWEYtOQIeM4AP1DE06KpSOlWeT5XqZBnc+j47mgUBRvKKWElyLkLKahfmo13DuHMjqtrBrunc0cCmkTgCEF4L/FeDzlhs0TSa5cwFkbHQiSHGn/xEx4Td8is/LyDi/rikxNTRU="

    OpenApiSpex.schema(%{
      description: "List of transactions to compose",
      type: :object,
      properties: %{
        transactions: %Schema{
          type: :lists,
          description:
            "List of base64 encoded, jammed nock representing a transaction"
        }
      },
      required: [:transactions],
      example: %{
        "transactions" => [@example_tx, @example_tx]
      }
    })
  end

  defmodule ComposeResult do
    # the intent is the result of
    #
    # Examples.ETransparent.ETransaction.swap_from_actions_non_eph_nullifier |>
    # Noun.Nounable.to_noun  |> Noun.Jam.jam() |> Base.encode64
    #
    # but it cannot be executed at compiletime due to the memoization.
    @example_tx "BcDCAKg21PQVEIpNQ4aM4wD0D004KZaOlGaR53uZBnU+j47ngkJRvKGUEl6KkLOYhvppmlYBUG2o6SsgFJuGDBnHAegPJpL9EQMk1OCEfg58Iy2rydpJHtHwgrMLIU4Ceg5lIVOzGkZ1QCg2Na0sq2FUD4W0APQoAP8txuMpN2yeSHLlAs7a6ECQ5Ej7J2bCa/oWmZWXd3hZV05NrSwAqjkZfQWEYtOQIeM4AP1DE06KpSOlWeT5XqZBnc+j47mgUBRvKKWElyLkLKahfmo13DuHMjqtrBrunc0cCmkTgCEF4L/FeDzlhs0TSa5cwFkbHQiSHGn/xEx4Td8is/LyDi/rikxNTRU="

    OpenApiSpex.schema(%{
      description: "Result of composing the list of transactions",
      type: :object,
      properties: %{
        transaction: %Schema{
          type: :string,
          default: "",
          description: "The composed transaction"
        }
      },
      required: [:transaction],
      example: %{
        "transaction" => @example_tx
      }
    })
  end

  defmodule VerifyRequest do
    # the intent is the result of
    #
    # Examples.ETransparent.ETransaction.swap_from_actions_non_eph_nullifier |>
    # Noun.Nounable.to_noun  |> Noun.Jam.jam() |> Base.encode64
    #
    # but it cannot be executed at compiletime due to the memoization.
    @example_tx "BcDCAKg21PQVEIpNQ4aM4wD0D004KZaOlGaR53uZBnU+j47ngkJRvKGUEl6KkLOYhvppmlYBUG2o6SsgFJuGDBnHAegPJpL9EQMk1OCEfg58Iy2rydpJHtHwgrMLIU4Ceg5lIVOzGkZ1QCg2Na0sq2FUD4W0APQoAP8txuMpN2yeSHLlAs7a6ECQ5Ej7J2bCa/oWmZWXd3hZV05NrSwAqjkZfQWEYtOQIeM4AP1DE06KpSOlWeT5XqZBnc+j47mgUBRvKKWElyLkLKahfmo13DuHMjqtrBrunc0cCmkTgCEF4L/FeDzlhs0TSa5cwFkbHQiSHGn/xEx4Td8is/LyDi/rikxNTRU="

    OpenApiSpex.schema(%{
      description: "Transaction to verify",
      type: :object,
      properties: %{
        transaction: %Schema{
          type: :string,
          default: "",
          description:
            "Base64 encoded, jammed nock representing a transaction"
        }
      },
      required: [:transaction],
      example: %{
        "transaction" => @example_tx
      }
    })
  end

  defmodule VerifyResult do
    OpenApiSpex.schema(%{
      description: "Verification result",
      type: :object,
      properties: %{
        valid?: %Schema{
          type: :boolean,
          description: "Boolean indicating whether the transaction is valid"
        }
      },
      required: [:valid?],
      example: %{
        "valid?" => true
      }
    })
  end
end
