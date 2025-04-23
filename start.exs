Logger.configure(level: :debug)

:net_kernel.start([:"primary2@127.0.0.1"])
:erlang.set_cookie(:mycookie)

:mnesia.stop()
:mnesia.start()

IO.inspect(:net)

eclient = Anoma.Client.Examples.EClient.create_example_client()

IO.inspect(:client)

http_port =
  Application.get_env(:anoma_client, Anoma.Client.Web.Endpoint)[:http][:port]

IO.puts("#{http_port} #{eclient.node.node_id}")
# uncomment this for debug messages
Logger.configure(level: :debug)

Anoma.Node.Utility.Consensus.start_link(
  node_id: eclient.node.node_id,
  interval: 500
)
