defmodule Anoma.CairoResource.Utils do
  @moduledoc """
  I am a bunch of utility functions useful for the Cairo backend.
  """

  require Logger

  @spec integer_or_hex_to_n_byte_binary(
          integer() | String.t(),
          non_neg_integer()
        ) :: {:ok, binary()} | {:error, term()}
  def integer_or_hex_to_n_byte_binary(value, n) do
    cond do
      is_integer(value) ->
        {:ok, <<value::size(n * 8)>>}

      is_binary(value) ->
        hex_to_n_byte_binary(value, n)

      true ->
        {:error, "Expected integer or hex string, got #{value}"}
    end
  end

  @spec hex_to_n_byte_binary(String.t(), non_neg_integer()) ::
          {:ok, binary()} | {:error, term()}
  def hex_to_n_byte_binary(hex_string, n) do
    hex_num_string_0 = String.replace_prefix(hex_string, "0x", "")

    hex_num_string =
      if rem(String.length(hex_num_string_0), 2) == 0 do
        hex_num_string_0
      else
        "0" <> hex_num_string_0
      end
      |> String.upcase()

    with {:ok, str} <- Base.decode16(hex_num_string) do
      {:ok, binary_padding(str, n)}
    else
      _ -> {:error, "Invalid hex string: #{hex_string}"}
    end
  end

  @spec binary_padding(binary(), integer()) :: binary()
  defp binary_padding(binary, size) when byte_size(binary) <= size do
    <<0::size((size - byte_size(binary)) * 8)>> <> binary
  end

  @spec binary_to_hex(binary()) :: String.t()
  def binary_to_hex(binary) do
    if binary == <<>> do
      "0x0"
    else
      "0x" <> Base.encode16(binary)
    end
  end

  @spec check_list(list({:ok | :error, any()})) ::
          {:ok, list(any())} | {:error, any()}
  def check_list(lst) do
    with true <-
           Enum.all?(lst, &(elem(&1, 0) == :ok)),
         lst_1 = Enum.map(lst, &elem(&1, 1)) do
      {:ok, lst_1}
    else
      _ -> Enum.find(lst, &(elem(&1, 0) == :error))
    end
  end

  @spec parse_json_field_to_binary32(Jason.OrderedObject.t(), String.t()) ::
          {:ok, binary()} | {:error, term()}
  def parse_json_field_to_binary32(json, field) do
    if json_object_has_nonempty_key(json, field) do
      integer_or_hex_to_n_byte_binary(json[field], 32)
    else
      {:error, "JSON object does not have a value for key #{field}"}
    end
  end

  @spec parse_json_optional_field_to_binary32(
          Jason.OrderedObject.t(),
          String.t(),
          binary()
        ) ::
          {:ok, binary()} | {:error, term()}
  def parse_json_optional_field_to_binary32(json, field, default) do
    if json_object_has_nonempty_key(json, field) do
      integer_or_hex_to_n_byte_binary(json[field], 32)
    else
      {:ok, default}
    end
  end

  @spec parse_json_field_to_boolean(Jason.OrderedObject.t(), String.t()) ::
          {:ok, boolean()} | {:error, term()}
  def parse_json_field_to_boolean(json, field) do
    if json_object_has_key(json, field) and is_boolean(json[field]) do
      {:ok, json[field]}
    else
      {:error, "JSON object does not have a boolean value for key #{field}"}
    end
  end

  @spec json_object_has_key(Jason.OrderedObject.t(), String.t()) :: boolean()
  def json_object_has_key(json_object, key) do
    Enum.any?(json_object, &(elem(&1, 0) == key))
  end

  @spec json_object_has_empty_key(Jason.OrderedObject.t(), String.t()) ::
          boolean()
  def json_object_has_empty_key(json_object, key) do
    json_object_has_key(json_object, key) and
      json_object[key] == ""
  end

  @spec json_object_has_nonempty_key(Jason.OrderedObject.t(), String.t()) ::
          boolean()
  def json_object_has_nonempty_key(json_object, key) do
    json_object_has_key(json_object, key) and
      json_object[key] != ""
  end
end
