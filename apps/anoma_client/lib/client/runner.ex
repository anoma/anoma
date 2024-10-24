defmodule Anoma.Client.Runner do
  @doc """
  I run the given Nock program with its inputs and return the result.
  """
  @spec prove(Noun.t(), [Noun.t()]) ::
          {:ok, Noun.t()} | {:error, :failed_to_prove}
  def prove(program, inputs) do
    core =
      (Noun.list_nock_to_erlang(program) ++ [Nock.rm_core()])
      |> to_improper_list()

    stuff = to_improper_list([6, 1] ++ inputs)

    case Nock.nock(core, [9, 2, 10, stuff, 0 | 1]) do
      {:ok, noun} ->
        {:ok, noun}

      :error ->
        {:error, :failed_to_prove}
    end
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  @doc """
  I turn a list into an improper list.
  E.g., [1,2,3] -> [1,2|3]
  """
  @spec to_improper_list([any()]) :: maybe_improper_list(any(), any())
  def to_improper_list([]), do: []
  def to_improper_list([x]), do: [x]
  def to_improper_list([x, y]), do: [x | y]
  def to_improper_list([h | t]), do: [h | to_improper_list(t)]
end
