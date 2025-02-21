defmodule Anoma.Client.Web.NockController do
  use Anoma.Client.Web, :controller

  alias Anoma.Client.Nock.Runner

  action_fallback(Anoma.Client.Web.FallbackController)

  @doc """
  I execute the given Nock program locally.
  """
  def run(conn, params = %{"inputs" => _, "program" => _}) do
    inputs = params["inputs"]
    program = params["program"]

    with {:ok, program} <- Base.decode64(program),
         {:ok, program} <- program_to_noun(program),
         {inputs, []} <- inputs_to_noun(inputs),
         {:ok, result, io} <- Runner.prove(program, inputs),
         io <- Enum.map(io, &Noun.Jam.jam/1),
         jammed <- Noun.Jam.jam(result) do
      render(conn, "run.json", result: jammed, io: io)
    else
      e ->
        IO.inspect(e, label: "else")
        e
    end
  end

  @doc """
  I execute the given Nock program locally.
  """
  def prove(
        conn,
        params = %{
          "public_inputs" => _,
          "private_inputs" => _,
          "program" => _
        }
      ) do
    priv_inputs = params["private_inputs"]
    publ_inputs = params["public_inputs"]
    program = params["program"]

    with {:ok, program} <- Base.decode64(program),
         {:ok, program} <- program_to_noun(program),
         {prv_inputs, []} <- inputs_to_noun(priv_inputs),
         {pub_inputs, []} <- inputs_to_noun(publ_inputs),
         {:ok, result, io} <- Runner.prove(program, pub_inputs ++ prv_inputs),
         io <- Enum.map(io, &Noun.Jam.jam/1),
         jammed <- Noun.Jam.jam(result) do
      render(conn, "prove.json", result: jammed, io: io)
    else
      e ->
        IO.inspect(e, label: "else")
        e
    end
  end

  ############################################################
  #                       Helpers                            #
  ############################################################

  # @doc """
  # I convert the program parameter to a noun.
  # """
  @spec program_to_noun(binary()) ::
          {:ok, Noun.t()} | {:error, :invalid_program}

  defp program_to_noun(program) do
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
  @spec inputs_to_noun([String.t()]) :: {list(Noun.t()), list(Noun.t())}
  defp inputs_to_noun(inputs) do
    inputs
    |> Enum.map(&Base.decode64!/1)
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
  @spec input_to_noun(binary()) ::
          {:ok, Noun.t()} | {:error, :invalid_input, any()}
  defp input_to_noun(input) do
    case Noun.Jam.cue(input) do
      {:ok, noun} ->
        {:ok, noun}

      _ ->
        {:error, :invalid_input, input}
    end
  end
end
