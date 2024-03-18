defmodule Anoma.Cli do
  @spec argument_parser() :: Optimus.t()
  def argument_parser() do
    Optimus.new!(
      name: "anoma",
      description: """
      Starts up Anoma.
      """,
      allow_unknown_args: false,
      parse_double_dash: true,
      args: [],
      options: [],
      subcommands: [
        nockma: Nock.Cli.argument_option(),
        submit: [
          name: "submit",
          options: [
            nodeid: [
              short: "-n",
              long: "--node-id",
              parser: :string,
              required: true],
            host: [
              short: "-h",
              long: "--address-host",
              parser: :string,
              required: :true],
            port: [
              short: "-p",
              long: "--address-port",
              parser: :string,
              required: :true],
            mempoolid: [
              short: "-m",
              long: "--mempool-id",
              parser: :string,
              required: :true],
            transportid: [
              short: "-t",
              long: "--transport-id",
              parser: :string,
              required: :true]],
          args: [
            infile: [
              required: true,
              parser: :string
            ]]],
        get_key: [
          name: "get-key",
          options: [
            nodeid: [
              short: "-n",
              long: "--node-id",
              parser: :string,
              required: true],
            host: [
              short: "-h",
              long: "--address-host",
              parser: :string,
              required: :true],
            port: [
              short: "-p",
              long: "--address-port",
              parser: :string,
              required: :true],
            storageid: [
              short: "-s",
              long: "--storage-id",
              parser: :string,
              required: :true],
            transportid: [
              short: "-t",
              long: "--transport-id",
              parser: :string,
              required: :true]],
          args: [
            key: [
              required: true,
              parser: :string
            ]]]
            ])
  end

  def submit(orouter, otransport, omempool, host, port, txfile) do
    {:ok, router} = Anoma.Node.Router.start
    {:ok, transport} = Anoma.Node.Router.start_transport(router)
    {:ok, proxy} = Anoma.Node.Router.start_engine(router, Anoma.Node.Proxy, router)
    orouter =    Anoma.Node.Router.id_to_addr(router, Anoma.Crypto.Id.read(orouter))
    otransport = Anoma.Node.Router.id_to_addr(router, Anoma.Crypto.Id.read(otransport))
    omempool =   Anoma.Node.Router.id_to_addr(router, Anoma.Crypto.Id.read(omempool))

    # teach our local transport engine about the topology of the remote node
    Anoma.Node.Router.cast(transport, {:learn_node, orouter.id, {:quic, :binary.bin_to_list(host), port}})
    Anoma.Node.Router.cast(transport, {:learn_engine, otransport.id, orouter.id})
    Anoma.Node.Router.cast(transport, {:learn_engine, omempool.id, orouter.id})

    # open connection to the remote node, and tell it about our local topology
    Anoma.Node.Router.cast(proxy, {:cast, otransport, {:learn_engine, proxy.id, router.id}})

    {:ok, parsed} = Noun.Format.parse(File.read!(txfile))
    Anoma.Node.Router.call(proxy, {:call, omempool, {:tx, {:kv, parsed}}})
  end

  def get_key(orouter, otransport, ostorage, host, port, key) do
    {:ok, router} = Anoma.Node.Router.start
    {:ok, transport} = Anoma.Node.Router.start_transport(router)
    {:ok, proxy} = Anoma.Node.Router.start_engine(router, Anoma.Node.Proxy, router)
    orouter =    Anoma.Node.Router.id_to_addr(router, Anoma.Crypto.Id.read(orouter))
    otransport = Anoma.Node.Router.id_to_addr(router, Anoma.Crypto.Id.read(otransport))
    ostorage =   Anoma.Node.Router.id_to_addr(router, Anoma.Crypto.Id.read(ostorage))

    # teach our local transport engine about the topology of the remote node
    Anoma.Node.Router.cast(transport, {:learn_node, orouter.id, {:quic, :binary.bin_to_list(host), port}})
    Anoma.Node.Router.cast(transport, {:learn_engine, otransport.id, orouter.id})
    Anoma.Node.Router.cast(transport, {:learn_engine, ostorage.id, orouter.id})

    # open connection to the remote node, and tell it about our local topology
    Anoma.Node.Router.cast(proxy, {:cast, otransport, {:learn_engine, proxy.id, router.id}})

    value = Anoma.Node.Router.call(proxy, {:call, ostorage, {:get, String.to_integer(key)}})
    IO.inspect(value)
    value
  end

  @spec main([binary()]) :: :ok
  def main(argv) do
    case Optimus.parse!(argument_parser(), argv) do
      {[:nockma], parsed} ->
        Nock.Cli.main(parsed)
      {[:submit],
        %{args: %{infile: infile},
          options: %{nodeid: nodeid, host: host, port: port, mempoolid: mempoolid, transportid: transportid}}} ->
        submit(nodeid, transportid, mempoolid, host, String.to_integer(port), infile)
      {[:get_key],
        %{args: %{key: key},
          options: %{nodeid: nodeid, host: host, port: port, storageid: storageid, transportid: transportid}}} ->
        get_key(nodeid, transportid, storageid, host, String.to_integer(port), key)
      parsed ->
        IO.inspect(parsed)
        IO.puts("Hello from Anoma. Sadly the Cli is barebones")
    end
  end
end
