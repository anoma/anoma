defmodule Anoma.Client.Api.Servers.Nock do
  alias Anoma.Client.Runner
  alias Anoma.Protobuf.Nock.Error
  alias Anoma.Protobuf.Nock.Input
  alias Anoma.Protobuf.Nock.Prove
  alias Anoma.Protobuf.Nock.Run
  alias Anoma.Protobuf.Nock.Success
  alias GRPC.Server.Stream

  use GRPC.Server, service: Anoma.Protobuf.NockService.Service

  @spec prove(Prove.Request.t(), Stream.t()) :: Prove.Response.t()
  def prove(request, _stream) do
    result =
      with {:ok, program} <- program_to_noun(request.program),
           {pub_inpt, []} <- inputs_to_noun(request.public_inputs),
           {prv_inpt, []} <- inputs_to_noun(request.private_inputs),
           {:ok, result, io} <- Runner.prove(program, pub_inpt ++ prv_inpt),
           io <- Enum.map(io, &Noun.Jam.jam/1),
           jammed <- Noun.Jam.jam(result) do
        {:ok, jammed, io}
      else
        {_, invalid_inputs} when is_list(invalid_inputs) ->
          {:error, "invalid inputs: #{inspect(invalid_inputs)}"}

        {:error, :invalid_program} ->
          {:error, "invalid program"}

        {:error, :failed_to_prove} ->
          {:error, "failed to prove"}

        _ ->
          {:error, "failed to evaluate"}
      end

    result =
      case result do
        {:ok, jammed, io} ->
          {:success, %Success{result: jammed, output: io}}

        {:error, reason} ->
          {:error, %Error{error: "#{reason}"}}
      end

    %Prove.Response{result: result}
  end

  @spec run(Run.Request.t(), Stream.t()) :: Run.Response.t()
  def run(request, _stream) do
    result =
      with {:ok, program} <- program_to_noun(request.program),
           {inputs, []} <- inputs_to_noun(request.inputs),
           {:ok, result, io} <- Runner.prove(program, inputs),
           io <- Enum.map(io, &Noun.Jam.jam/1),
           jammed <- Noun.Jam.jam(result) do
        {:ok, jammed, io}
      else
        {_, invalid_inputs} when is_list(invalid_inputs) ->
          {:error, "invalid inputs: #{inspect(invalid_inputs)}"}

        {:error, :invalid_program} ->
          {:error, "invalid program"}

        {:error, :failed_to_prove} ->
          {:error, "failed to prove"}

        _ ->
          {:error, "failed to evaluate"}
      end

    result =
      case result do
        {:ok, jammed, io} ->
          {:success, %Success{result: jammed, output: io}}

        {:error, reason} ->
          {:error, %Error{error: "#{reason}"}}
      end

    %Run.Response{result: result}
  end

  #############################################################################
  #                                Helpers                                    #
  #############################################################################

  # @doc """
  # I convert the program parameter to a noun.
  # """
  @spec program_to_noun(
          {:jammed_program, binary()}
          | {:text_program, String.t()}
        ) :: {:ok, Noun.t()} | {:error, :invalid_program}
  defp program_to_noun({:text_program, program}) do
    case Noun.Format.parse(program) do
      {:ok, noun} ->
        {:ok, noun}

      _ ->
        {:error, :invalid_program}
    end
  end

  defp program_to_noun({:jammed_program, program}) do
    case Noun.Jam.cue(program) do
      {:ok, noun} ->
        {:ok, noun}

      _ ->
        {:error, :invalid_program}
    end
  end

  # @doc """
  # I convert a list of inputs to nouns.

  # I return a tuple with the successful and failed conversions.
  # """
  @spec inputs_to_noun([Input.t()]) :: {list(Noun.t()), list(Noun.t())}
  defp inputs_to_noun(inputs) do
    inputs
    |> Enum.map(&input_to_noun/1)
    |> Enum.reduce({[], []}, fn input, {valid, invalid} ->
      case input do
        {:ok, input} ->
          {[input | valid], invalid}

        {:error, _, input} ->
          {valid, [input | invalid]}
      end
    end)
    |> then(fn {valid, invalid} ->
      {Enum.reverse(valid), Enum.reverse(invalid)}
    end)
  end

  # @doc """
  # I turn an input into a noun.
  # """
  @spec input_to_noun(Input.t()) ::
          {:ok, Noun.t()} | {:error, :invalid_input, any()}
  defp input_to_noun(%{input: {:jammed, input}}) do
    case Noun.Jam.cue(input) do
      {:ok, noun} ->
        {:ok, noun}

      _ ->
        {:error, :invalid_input, input}
    end
  end

  defp input_to_noun(%{input: {:text, input}}) do
    case Noun.Format.parse(input) do
      {:ok, noun} ->
        {:ok, noun}

      _ ->
        {:error, :invalid_input, input}
    end
  end
end
