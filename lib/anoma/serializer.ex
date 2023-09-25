# TODO rethink name!?
defmodule Anoma.Serializer do
  @moduledoc """
  I aspire to give a language independent serialization format for any
  erlang/elixir type. Further, I handle hashing and digesting the
  terms as well!

  For the time being, Ι just use the basic erlang only technique,
  please improve me!
  """

  @type private_key() :: [:crypto.key_id()]
  @type public_key() :: [:crypto.key_id()]

  @spec serialize(:erlang.term()) :: binary()
  def serialize(object) do
    :erlang.term_to_binary(object)
  end

  @doc """
  I `deserialize` the given object back into an erlang term.
  """
  @spec deserialize(binary()) :: :erlang.term()
  def deserialize(object) do
    :erlang.binary_to_term(object)
  end

  @spec digest(:erlang.term()) :: binary()
  def digest(object) do
    :crypto.hash(:blake2b, serialize(object))
  end

  @doc """
  I `deserialize` the given object back into an erlang term.
  """
  @spec sign(:erlang.term(), private_key()) :: binary()
  def sign(object, key) do
    :crypto.sign(:rsa, :ripemd160, object, key)
  end

  @spec verify(binary(), public_key(), binary()) :: boolean()
  def verify(message, pub_key, signature) do
    :crypto.verify(:rsa, :ripemd160, message, signature, pub_key)
  end
end
