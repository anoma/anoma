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

    {:ok, result, _stdio} = Runner.prove(program, inputs)

    assert result == 9

    {:ok, result}
  end

  @spec squared_without_arguments() :: {:ok, Noun.t()} | :error
  def squared_without_arguments() do
    {:ok, program} =
      :code.priv_dir(:anoma_client)
      |> Path.join("test_juvix/Squared.nockma")
      |> File.read!()
      |> Nock.Cue.cue()

    inputs = []

    {:ok, result, _stdio} = Runner.prove(program, inputs)

    assert result == 0

    {:ok, result}
  end

  @spec prove_squared_small() :: any()
  def prove_squared_small() do
    # jammed base64 encoded square function that takes in one parameter
    {:ok, program} =
      "BcGCZJgJ7v9BMmQQLewS4uPxRKY="
      |> Base.decode64!()
      |> Nock.Cue.cue()

    inputs = Enum.map(["3"], &Noun.Format.parse_always/1)
    {:ok, result, _stdio} = Runner.prove(program, inputs)

    assert result == 9

    result
  end

  @spec prove_with_hints() :: {:ok, Noun.t(), [String.t()]} | :error
  def prove_with_hints() do
    {:ok, program} =
      :code.priv_dir(:anoma_client)
      |> Path.join("test_juvix/Tracing.nockma")
      |> File.read!()
      |> Nock.Cue.cue()

    inputs = []

    {:ok, result, stdio} = Runner.prove(program, inputs)

    assert result == 0
    assert stdio == [1, 4, 2, 4]

    {:ok, result, stdio}
  end

  @spec prove_with_hint() :: {:ok, any(), any()}
  def prove_with_hint() do
    {:ok, program} =
      :code.priv_dir(:anoma_client)
      |> Path.join("test_juvix/Identity.nockma")
      |> File.read!()
      |> Nock.Cue.cue()

    inputs =
      Enum.map(["3"], &Noun.Format.parse_always/1)

    {:ok, result, stdio} = Runner.prove(program, inputs)

    assert result == 3
    assert stdio == [6_513_249]

    {:ok, result, stdio}
  end

  @spec prove_with_cell_hint() :: {:ok, any(), any()}
  def prove_with_cell_hint() do
    {:ok, program} =
      :code.priv_dir(:anoma_client)
      |> Path.join("test_juvix/CellHint.nockma")
      |> File.read!()
      |> Nock.Cue.cue()

    inputs =
      Enum.map(["3"], &Noun.Format.parse_always/1)

    {:ok, result, stdio} = Runner.prove(program, inputs)

    assert result == [1, 2 | 0]
    assert stdio == [1, 0, 0, [1 | 0], 1, [1, 2 | 0]]

    {:ok, result, stdio}
  end
end
