defmodule Anoma.Client.Examples.EProve do
  alias Anoma.Client.Runner

  require ExUnit.Assertions
  import ExUnit.Assertions

  @doc """
  I run the Juvix program "Squared" using the prove function.
  """
  @spec prove_squared() :: {:ok, Noun.t()} | :error
  def prove_squared() do
    {:ok, program} =
      :code.priv_dir(:anoma_client)
      |> Path.join("test_juvix/Squared.nockma")
      |> File.read!()
      |> Nock.Cue.cue()

    inputs =
      Enum.map(["3"], &Noun.Format.parse_always/1)

    result = Runner.prove(program, inputs)

    assert result == {:ok, 9}

    result
  end

  def prove_squared_small() do
    # jammed base64 encoded square function that takes in one parameter
    {:ok, program} =
      "BcGCZJgJ7v9BMmQQLewS4uPxRKY="
      |> Base.decode64!()
      |> Nock.Cue.cue()

    inputs = Enum.map(["3"], &Noun.Format.parse_always/1)
    result = Runner.prove(program, inputs)

    assert result == {:ok, 9}

    result
  end
end
