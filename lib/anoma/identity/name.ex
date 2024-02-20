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

  @doc """
  Adds the given key to the given namespace. The signer who owns the
  namespace must have signed.
  """
  @spec add(t(), binary(), {list(binary()), binary()}) ::
          :ok | :no_namespace | :failed_placement | :improper_data
  def add(namespace = %__MODULE__{}, sig, d = {name, new_key})
      when is_list(name) do
    store = namespace.storage
    storage_space = [name_space() | name]

    with {:ok, pub} <- Storage.get(store, [name_space(), hd(name)]),
         true <- Verification.verify_request(sig, d, pub),
         :absent <- Storage.get(namespace.storage, storage_space),
         {:atomic, :ok} <- Storage.put(store, storage_space, new_key) do
      :ok
    else
      {:ok, _} -> :already_there
      :absent -> :no_namespace
      {:aborted, _} -> :failed_placement
      false -> :improper_data
    end
  end

  @spec all_identities(t(), binary()) :: MapSet.t(Id.Extern.t())
  def all_identities(namespace = %__MODULE__{}, name) when is_binary(name) do
    Storage.get_keyspace(namespace.storage, [name_space(), name])
    |> Stream.map(fn {_, id} -> id end)
    |> MapSet.new()
  end

  ############################################################
  #                           Helpers                        #
  ############################################################
  @base_name_space "name"
  def name_space, do: @base_name_space
end
