defmodule Anoma.Client.CLI do
  @moduledoc """
  I implement the logic for the Anoma client commandline interface.


  ## Usage

  The executable expects the following arguments:

  - `--listen-port` - The port on which the client will listen for incoming connections.
  - `--node-host`   - The host of the remote node.
  - `--node-port`   - The port of the remote node.
  - `--node-id`     - The id of the remote node.

  For example, assuming an Anoma node running on ip 'localhost' and port '4000', start the client
  with the following command.

  ```shell
  ./anoma_client --listen-port 4001 --node-host localhost --node-port 4000 --node-id 123456
  ```

  ## Running a node

  Running a node can be done using the same repository. In an Elixir shell of the Anoma repo,
  run the following.

  ```text
  iex(1)> Anoma.Node.Examples.ENode.start_node(grpc_port: 8181)
  %Anoma.Node.Examples.ENode{
    grpc_port: 8181,
    tcp_ports: [],
    pid: #PID<0.353.0>,
    node_id: "110532251"
  }
  ```

  Use the value of `grpc_port` to point your client to the node.

  The client will connect to the remote node and start listening for incoming connections on port
  4001.

  ## Building

  To build the binary for the client run the following from the root of this repository.

  ```shell
  mix do --app anoma_client escript.build
  ```
  """

  alias Anoma.Client

  require Logger

  @spec main([String.t()]) :: any()
  def main(args) do
    case parse_args(args) do
      {:ok, args} ->
        start_node(args)

      {:error, err} ->
        IO.puts(show_error({:error, err}))
    end
  end

  @doc """
  Given a list of arguments, I start a new connection to a remote node.

  ## Options
  - `:listen_port` - The port on which the client will listen for incoming connections.
  - `:node_host` - The host of the remote node.
  - `:node_port` - The port of the remote node.
  """
  @spec start_node(Keyword.t()) :: any()
  def start_node(args) do
    case Client.connect(
           args[:node_host],
           args[:node_port],
           args[:listen_port],
           args[:node_id]
         ) do
      {:error, :node_unreachable} ->
        terminate(show_error({:error, :node_unreachable}))

      {:ok, client} ->
        IO.puts("Connected to node. Listening on port #{client.grpc_port}")
        Process.sleep(:infinity)
    end
  end

  @doc """
  I parse the arguments into a keyword list and validate if all the arguments are present.
  """
  @spec parse_args([String.t()]) ::
          {:ok, Keyword.t()}
          | {:error, {:invalid_args, [String.t()]}}
          | {:error, {:missing_args, [atom()]}}
  def parse_args(args) do
    arg_spec = [
      listen_port: :integer,
      node_host: :string,
      node_port: :integer
    ]

    case OptionParser.parse(args, strict: arg_spec) do
      {args, [], []} ->
        validate_args(args)

      {_, _rest, invalid} ->
        {:error, {:invalid_args, invalid}}
    end
  end

  @doc """
  Given a keyword list, I check if all required arguments are present.
  I return a list of missing arguments if theyre not present.
  """
  @spec validate_args(Keyword.t()) ::
          {:ok, Keyword.t()} | {:error, {:missing_args, [atom()]}}
  def validate_args(args) do
    required_args = [:listen_port, :node_host, :node_port]

    case Enum.group_by(required_args, &Keyword.has_key?(args, &1)) do
      %{false: missing} ->
        {:error, {:missing_args, missing}}

      %{true: _} ->
        {:ok, args}
    end
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # @doc """
  # I print various error messages to string in a human-readable format.
  # """
  @spec show_error({:error, term()}) :: String.t()
  defp show_error({:error, {:invalid_args, invalid}}) do
    argument_messages =
      Enum.map(invalid, fn {arg, value} ->
        "'#{arg}', value: '#{value}'"
      end)

    """
    Invalid arguments values given for
    #{Enum.intersperse(argument_messages, "\n") |> Enum.join("")}
    """
  end

  defp show_error({:error, {:missing_args, missing}}) do
    argumnets =
      Enum.map(missing, fn missing ->
        missing
        |> Atom.to_string()
        |> String.replace("_", "-")
        |> (fn x -> "--#{x}" end).()
      end)

    """
    Missing arguments: #{Enum.join(argumnets, ", ")}
    """
  end

  defp show_error({:error, :node_unreachable}) do
    "The remote node is unreachable. Are the hostname and port for the node correct?"
  end

  @spec terminate(term()) :: no_return()
  defp terminate(reason) do
    IO.puts("Client exiting")

    IO.puts("Reason: #{reason}")

    System.halt()

    :ok
  end
end
