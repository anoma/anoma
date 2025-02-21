defmodule Anoma.Client.Web.NockJSON do
  def render("run.json", %{result: res, io: io}) do
    %{result: Base.encode64(res), io: Enum.map(io, &Base.encode64/1)}
  end

  def render("prove.json", %{result: res, io: io}) do
    %{result: Base.encode64(res), io: Enum.map(io, &Base.encode64/1)}
  end
end
