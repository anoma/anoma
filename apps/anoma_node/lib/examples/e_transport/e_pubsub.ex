defmodule Anoma.Node.Examples.EPubSub do
  alias Anoma.Node.Event
  alias Anoma.Node.Examples.EAdvertise
  alias Anoma.Node.Transport.Proxy.Events

  import ExUnit.Assertions

  use EventBroker.WithSubscription

  @doc """
  I start two nodes that advertise to eachother.
  """
  def subscribe do
    # create two nodes in a distributed setting and advertise them to eachother.
    {local, remote, slave} =
      EAdvertise.seed_nodes_distributed(stop_slave: false)

    # the node proxy by default subscribes to all external events.
    # This means that if I send an event here, the remote node should receive it.
    # spawn a plain Elixir process on the remote node to listen for events.
    # This process will run in the second VM and send us a message if it received the message.
    # todo: this should eventually be replaced by a piece of domain logic that
    #       actually uses external events but we dont have that right now, so
    #       this is temporary workaround.
    this_proc = self()

    Node.spawn(slave, fn ->
      with_subscription [[]] do
        receive do
          %{body: %{body: "hello, world"}} ->
            # this sleep is "necessary" because the event is handled faster than
            # the grpc connection being closed and this will cause spurious
            # errors.
            Process.sleep(100)
            send(this_proc, :ok)
        end
      end
    end)

    # create an arbitrary external event sent from the local node
    event = %EventBroker.Event{
      source_module: nil,
      body: %Event{
        node_id: local.node_id,
        body: %Events.External{event: "hello, world"}
      }
    }

    # fire the event locally
    EventBroker.event(event)

    # ensure we received ack from other VM
    assert_receive :ok, 1000

    EAdvertise.stop_slave(slave)

    # ensure the remote node received this event
    {local, remote, slave}
  end
end
