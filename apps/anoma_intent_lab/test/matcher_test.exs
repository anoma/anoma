defmodule AnomaIntentLab.MatcherTest do
  use ExUnit.Case, async: true
  alias AnomaIntentLab.{Intent, Matcher}

  test "two-party match works" do
    a = %Intent{owner: :a, give: %{USD: 10}, want: %{EUR: 10}, constraints: %{domain: "x"}}
    b = %Intent{owner: :b, give: %{EUR: 10}, want: %{USD: 10}, constraints: %{domain: "x"}}
    assert {:ok, %{type: :two_party}} = Matcher.two_party(a, b)
  end

  test "domain mismatch blocks match" do
    a = %Intent{owner: :a, give: %{USD: 10}, want: %{EUR: 10}, constraints: %{domain: "x"}}
    b = %Intent{owner: :b, give: %{EUR: 10}, want: %{USD: 10}, constraints: %{domain: "y"}}
    assert :nomatch = Matcher.two_party(a, b)
  end

  test "three-cycle match works" do
    a = %Intent{owner: :a, give: %{USD: 10}, want: %{EUR: 10}, constraints: %{domain: "x"}}
    b = %Intent{owner: :b, give: %{EUR: 10}, want: %{JPY: 10}, constraints: %{domain: "x"}}
    c = %Intent{owner: :c, give: %{JPY: 10}, want: %{USD: 10}, constraints: %{domain: "x"}}
    assert {:ok, %{type: :three_cycle}} = Matcher.three_cycle([a, b, c])
  end
end
