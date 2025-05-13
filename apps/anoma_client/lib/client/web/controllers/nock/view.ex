defmodule Anoma.Client.Web.NockJSON do
  def render("run.json", %{result: res, io: io}) do
    %{result: Base.encode64(res), io: Enum.map(io, &Base.encode64/1)}
  end

  def render("prove.json", %{result: res, io: io}) do
    %{result: Base.encode64(res), io: Enum.map(io, &Base.encode64/1)}
  end

  def render("error.json", %{io: io, reason: reason}) do
    trunc =
      case reason do
        {reason, _} -> reason
        {reason, _, _} -> reason
        {reason, _, _, _} -> reason
        reason -> reason
      end

    %{result: "error", io: Enum.map(io, &Base.encode64/1), reason: trunc}
  end
end
