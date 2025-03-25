defmodule MyServer do
  use GenServer

  def start_link(state \\ []) do
    IO.puts("#{__MODULE__} #{inspect(self())} start_link")
    GenServer.start_link(__MODULE__, state)
  end

  @doc """
  GenServer.init/1 callback
  """
  def init(state) do
    IO.puts("#{__MODULE__} #{inspect(self())} init")
    {:ok, state}
  end

  def handle_call(:do_something, from, state) do
    spawn_link(fn ->
      IO.inspect("starting work")
      Process.sleep(50000)
      GenServer.reply(from, :done)
    end)

    {:noreply, state}
  end

  def handle_cast({:do_something, from}, state) do
    spawn_link(fn ->
      IO.puts("starting")
      Process.sleep(50000)
      send(from, :done)
    end)

    {:noreply, state}
  end

  def test() do
    {:ok, s} = GenServer.start(MyServer, [])

    spawn_link(fn ->
      result = GenServer.call(s, :do_something, :infinity)
      IO.inspect(result)
    end)

    s
  end

  def test2() do
    {:ok, s} = GenServer.start(MyServer, [])

    spawn_link(fn ->
      GenServer.cast(s, {:do_something, self()})

      receive do
        m ->
          IO.inspect(m)
      end
    end)

    s
  end
end
