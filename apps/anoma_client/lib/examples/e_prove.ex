defmodule Anoma.Client.Examples.EProve do
  @moduledoc """
  I contain functions that test and showcase the Runer module.

  I test and run nock programs and verify their outputs.
  """
  alias Anoma.Client.Nock.Runner

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
      |> Noun.Jam.cue()

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
      |> Noun.Jam.cue()

    inputs = []

    {:ok, result, _stdio} = Runner.prove(program, inputs)

    assert result == 0

    {:ok, result}
  end

  @spec prove_squared_small() :: any()
  def prove_squared_small() do
    # jammed base64 encoded square function that takes in one parameter
    {:ok, program} =
      square_endpoint_call()
      |> Base.decode64!()
      |> Noun.Jam.cue()

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
      |> Noun.Jam.cue()

    inputs = []

    {:ok, result, stdio} = Runner.prove(program, inputs)

    assert result == <<>>
    assert stdio == [<<1>>, <<4>>, <<2>>, <<4>>]

    {:ok, result, stdio}
  end

  @spec prove_with_hint() :: {:ok, any(), any()}
  def prove_with_hint() do
    {:ok, program} =
      :code.priv_dir(:anoma_client)
      |> Path.join("test_juvix/Identity.nockma")
      |> File.read!()
      |> Noun.Jam.cue()

    inputs =
      Enum.map(["3"], &Noun.Format.parse_always/1)

    {:ok, result, stdio} = Runner.prove(program, inputs)

    assert result == 3
    assert stdio == ["abc"]

    {:ok, result, stdio}
  end

  @spec prove_with_cell_hint() :: {:ok, any(), any()}
  def prove_with_cell_hint() do
    {:ok, program} =
      :code.priv_dir(:anoma_client)
      |> Path.join("test_juvix/CellHint.nockma")
      |> File.read!()
      |> Noun.Jam.cue()

    inputs =
      Enum.map(["3"], &Noun.Format.parse_always/1)

    {:ok, result, stdio} = Runner.prove(program, inputs)

    assert result == [1, 2 | <<>>]

    assert stdio == [
             <<1>>,
             <<>>,
             <<>>,
             [<<1>> | <<>>],
             <<1>>,
             [<<1>>, <<2>> | <<>>]
           ]

    {:ok, result, stdio}
  end

  @spec square_endpoint_call() :: binary()
  defp square_endpoint_call() do
    layer_depth = (Nock.Lib.stdlib_layers() + 2) |> Noun.index_to_offset()

    "[[8 [9 4 0 #{layer_depth}] 9 2 10 [6 [0 14] 0 14] 0 2] 0 0]"
    |> Noun.Format.parse_always()
    |> Noun.Jam.jam()
    |> Base.encode64()
  end
end
