defmodule Anoma.Client.Web.NockController do
  use Anoma.Client.Web, :controller
  use OpenApiSpex.ControllerSpecs

  alias Anoma.Client.Runner
  alias Anoma.Client.Web.NockController.Spec

  action_fallback(Anoma.Client.Web.FallbackController)

  ############################################################
  #                           OpenAPI Spec                   #
  ############################################################

  tags(["Nock"])

  operation(:run,
    summary: "Run a Nock program",
    parameters: [],
    request_body:
      {"Nock program to run", "application/json", Spec.RunRequest},
    responses: [
      ok: {"Evaluation result", "application/json", Spec.Result}
    ]
  )

  tags(["Nock"])

  operation(:prove,
    summary: "Prove a Nock program",
    parameters: [],
    request_body:
      {"Nock program to prove", "application/json", Spec.ProveRequest},
    responses: [
      ok: {"Evaluation result", "application/json", Spec.Result}
    ]
  )

  ############################################################
  #                          Actions                         #
  ############################################################

  @doc """
  I execute the given Nock program locally.
  """
  def run(conn, params = %{"inputs" => _, "program" => _}) do
    inputs = params["inputs"]
    program = params["program"]

    with {:ok, program} <- Base.decode64(program),
         {:ok, program} <- program_to_noun(program),
         {inputs, []} <- parse_inputs(inputs),
         {:ok, result, io} <- Runner.prove(program, inputs),
         io <- Enum.map(io, &Noun.Jam.jam/1),
         jammed <- Noun.Jam.jam(result) do
      render(conn, "run.json", result: jammed, io: io)
    else
      {:error, :failed_to_prove, reason, hints} ->
        render(conn, "error.json",
          io: Enum.map(hints, &Noun.Jam.jam/1),
          reason: reason
        )

      e ->
        e
    end
  end

  @doc """
  I execute the given Nock program locally.
  """
  def prove(conn, params = %{"program" => _}) do
    priv_inputs = Map.get(params, "private_inputs", [])
    publ_inputs = Map.get(params, "public_inputs", [])
    program = params["program"]

    with {:ok, program} <- Base.decode64(program),
         {:ok, program} <- program_to_noun(program),
         {prv_inputs, []} <- parse_inputs(priv_inputs),
         {pub_inputs, []} <- parse_inputs(publ_inputs),
         {:ok, result, io} <- Runner.prove(program, pub_inputs ++ prv_inputs),
         io <- Enum.map(io, &Noun.Jam.jam/1),
         jammed <- Noun.Jam.jam(result) do
      render(conn, "prove.json", result: jammed, io: io)
    else
      {:error, :failed_to_prove, reason, hints} ->
        render(conn, "error.json",
          io: Enum.map(hints, &Noun.Jam.jam/1),
          reason: reason
        )

      e ->
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
  @spec parse_inputs([String.t()]) ::
          {[Noun.t() | binary()], [Noun.t() | binary()]}
  defp parse_inputs(inputs) do
    inputs
    |> Enum.map(&parse_input/1)
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

  @doc """
  Parses an input to either a noun, or a raw binary.
  """
  @spec parse_input(%{String.t() => term()}) ::
          {:ok, Noun.t() | binary()}
          | {:error, :invalid_base_encoding | :invalid_jammed_noun, term()}
  defp parse_input(%{"raw" => input}) do
    case Base.decode64(input) do
      {:ok, binary} ->
        {:ok, binary}

      :error ->
        {:error, :invalid_base_encoding, input}
    end
  end

  defp parse_input(%{"noun" => input}) do
    case Base.decode64(input) do
      {:ok, decoded} ->
        case Noun.Jam.cue(decoded) do
          {:ok, noun} ->
            {:ok, noun}

          _ ->
            {:error, :invalid_jammed_noun, input}

          :error ->
            {:error, :invalid_base_encoding, input}
        end
    end
  end
end
