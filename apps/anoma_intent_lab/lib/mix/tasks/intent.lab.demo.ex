defmodule Mix.Tasks.Intent.Lab.Demo do
  use Mix.Task
  @shortdoc "Runs a small intent-matching demo (two-party & three-cycle)"
  @moduledoc false

  alias AnomaIntentLab.{Intent, Matcher}

  @impl true
  def run(_args) do
    IO.puts("== Two-party demo ==")
    a = %Intent{owner: :alice, give: %{USDT: 10}, want: %{ATOM: 10}, constraints: %{domain: "local:demo"}}
    b = %Intent{owner: :bob,   give: %{ATOM: 10}, want: %{USDT: 10}, constraints: %{domain: "local:demo"}}
    IO.inspect(Matcher.two_party(a, b))

    IO.puts("\n== Three-cycle demo ==")
    c = %Intent{owner: :carol, give: %{OSMO: 10}, want: %{USDT: 10}, constraints: %{domain: "local:demo"}}
    d = %Intent{owner: :dave,  give: %{USDT: 10}, want: %{OSMO: 10}, constraints: %{domain: "local:demo"}}
    IO.inspect(Matcher.three_cycle([b, d, c]))
  end
end
