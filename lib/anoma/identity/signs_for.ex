defmodule Anoma.Identity.SignsFor do
  alias Anoma.Identity.Verification
  alias Anoma.Storage
  alias Anoma.Crypto.Id

  @spec sign_for(Storage.t(), Id.Extern.t(), Id.Extern.t(), binary()) ::
          :key_not_verified | :ok | :could_not_update_storage
  def sign_for(tab = %Storage{}, our_key, trusted_key, signature) do
    if Verification.verify_request(signature, trusted_key, our_key, tab) do
      key_space = [name_space(), our_key]

      new_set =
        case Storage.get(tab, key_space) do
          {:ok, set} -> MapSet.put(set, trusted_key)
          :absent -> MapSet.new([trusted_key])
        end

      case Storage.put(tab, key_space, new_set) do
        {:atomic, :ok} -> :ok
        _ -> :could_not_update_storage
      end
    else
      :key_not_verified
    end
  end

  @spec known(Storage.t(), Id.Extern.t()) :: MapSet.t(Id.Extern.t())
  def known(tab = %Storage{}, key) do
    case Storage.get(tab, [name_space(), key]) do
      {:ok, set} -> set
      :absent -> MapSet.new()
    end
  end

  @spec signs_for?(Storage.t(), Id.Extern.t(), Id.Extern.t()) :: boolean
  def signs_for?(tab, our_key, key_in_question) do
    known(tab, our_key)
    |> MapSet.member?(key_in_question)
  end

  @base_name_space "signs_for"
  def name_space, do: @base_name_space
end
