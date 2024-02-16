defmodule Anoma.Identity.Name do
  use TypedStruct
  alias Anoma.Storage
  alias Anoma.Crypto.Id
  alias Anoma.Identity.Verification

  typedstruct do
    field(:storage, Storage.t())
    field(:keyspace, atom())
  end

  @spec reserve_namespace(t(), binary(), Id.Extern.t(), binary()) ::
          :already_there | :improper_data | :ok | :failed_placement
  def reserve_namespace(namespace = %__MODULE__{}, name, pub, cdata)
      when is_binary(name) do
    with true <- Verification.verify_request(cdata, name, pub),
         :absent <- Storage.get(namespace.storage, [name_space(), name]),
         {:atomic, :ok} <-
           Storage.put(namespace.storage, [name_space(), name], pub) do
      :ok
    else
      {:ok, _} -> :already_there
      false -> :improper_data
      {:aborted, _} -> :failed_placement
    end
  end

  ############################################################
  #                           Helpers                        #
  ############################################################
  @base_name_space "name"
  def name_space, do: @base_name_space
end
