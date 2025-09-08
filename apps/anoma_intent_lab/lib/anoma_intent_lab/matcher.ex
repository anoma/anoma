defmodule AnomaIntentLab.Matcher do
  @moduledoc """
  Basit eşleştirici:
    - two_party/2: iki intent arasında karşılıklı kapsama varsa eşleşme
    - three_cycle/1: 3'lü döngü (A->B, B->C, C->A) varsa eşleşme
    - Domain kısıtı: domain belirtilmişse tüm taraflarda aynı olmalı
  """
  alias AnomaIntentLab.Intent

  @spec complement?(Intent.t(), Intent.t()) :: boolean()
  def complement?(%Intent{} = a, %Intent{} = b) do
    same_domain?(a, b) and covers?(a.give, b.want) and covers?(b.give, a.want)
  end

  def two_party(a, b) do
    if complement?(a, b) do
      {:ok, %{type: :two_party, actors: [a.owner, b.owner], transfer: settlement(a, b)}}
    else
      :nomatch
    end
  end

  defp settlement(a, b) do
    ab =
      for {asset, qty} <- a.want, qty > 0 do
        %{from: b.owner, to: a.owner, asset: asset, qty: qty}
      end

    ba =
      for {asset, qty} <- b.want, qty > 0 do
        %{from: a.owner, to: b.owner, asset: asset, qty: qty}
      end

    ab ++ ba
  end

  def three_cycle(intents) when is_list(intents) do
    intents
    |> combinations(3)
    |> Enum.find_value(:nomatch, fn [a, b, c] ->
      if same_domain?(a, b) and same_domain?(b, c) and same_domain?(a, c) and
           covers?(a.give, c.want) and covers?(b.give, a.want) and covers?(c.give, b.want) do
        {:ok,
         %{
           type: :three_cycle,
           actors: [a.owner, b.owner, c.owner],
           transfer:
             [
               for({asset, qty} <- a.want, do: %{from: b.owner, to: a.owner, asset: asset, qty: qty}),
               for({asset, qty} <- b.want, do: %{from: c.owner, to: b.owner, asset: asset, qty: qty}),
               for({asset, qty} <- c.want, do: %{from: a.owner, to: c.owner, asset: asset, qty: qty})
             ]
             |> List.flatten()
         }}
      else
        false
      end
    end)
  end

  defp covers?(have, need) do
    Enum.all?(need, fn {asset, qty} -> Map.get(have, asset, 0) >= qty end)
  end

  defp same_domain?(a, b) do
    da = Intent.domain(a)
    db = Intent.domain(b)
    is_nil(da) or is_nil(db) or da == db
  end

  # basit kombinasyon üretici
  defp combinations(list, 0), do: [[ ] |> Enum.reverse()]
  defp combinations([], _k), do: []
  defp combinations([h | t], k) when k > 0 do
    with_h = Enum.map(combinations(t, k - 1), &[h | &1])
    without_h = combinations(t, k)
    with_h ++ without_h
  end
end
