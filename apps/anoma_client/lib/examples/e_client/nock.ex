defmodule Anoma.Client.Examples.EClient.Nock do
  @moduledoc """
  I contain functions to test the public interface of the client.

  I show how to use the nock run and prove endpoints.
  """
  use Anoma.Client.Web.ConnCase
  use TypedStruct

  alias Anoma.Client.Examples.EClient

  import ExUnit.Assertions
  import Anoma.Client.Examples.EClient

  @doc """
  I run a simple nock program via the api.
  """
  @spec run_nock(EClient.t()) :: {EClient.t(), String.t()}
  def run_nock(client \\ setup()) do
    program = Base.encode64(squared())

    # the json payload the endpoint expects
    payload = %{"inputs" => [], "program" => program}

    data =
      client.conn
      |> post(~p"/nock/run", payload)
      |> json_response(200)

    expected_io = []

    expected_result = Base.encode64(Noun.Jam.jam(<<0>>))

    assert data == %{
             "io" => expected_io,
             "result" => expected_result
           }

    client
  end

  @doc """
  I run a simple nock program via the api with a single argument.
  """
  @spec run_nock_with_arg(EClient.t()) :: {EClient.t(), String.t()}
  def run_nock_with_arg(client \\ setup()) do
    program = Base.encode64(squared())
    inputs = [Base.encode64(Noun.Jam.jam(3))]

    # the json payload the endpoint expects
    payload = %{"inputs" => inputs, "program" => program}

    data =
      client.conn
      |> post(~p"/nock/run", payload)
      |> json_response(200)

    expected_io = []

    expected_result = Base.encode64(Noun.Jam.jam(<<9>>))

    assert data == %{
             "io" => expected_io,
             "result" => expected_result
           }

    client
  end

  @doc """
  I run a siple nock program that is supposed to return some io.
  """
  @spec run_nock_with_io(EClient.t()) :: {EClient.t(), String.t()}
  def run_nock_with_io(client \\ setup()) do
    program = Base.encode64(tracing())
    # the json payload the endpoint expects
    payload = %{"inputs" => [], "program" => program}

    data =
      client.conn
      |> post(~p"/nock/run", payload)
      |> json_response(200)

    expected_io =
      [<<1>>, <<4>>, <<2>>, <<4>>]
      |> Enum.map(&Noun.Jam.jam/1)
      |> Enum.map(&Base.encode64/1)

    expected_result = Base.encode64(Noun.Jam.jam(<<0>>))

    assert data == %{
             "io" => expected_io,
             "result" => expected_result
           }

    client
  end

  @doc """
  I add a transaction to the mempool.
  """
  @spec prove_nock(EClient.t()) :: {EClient.t(), String.t()}
  def prove_nock(client \\ setup()) do
    program = Base.encode64(squared())

    # the json payload the endpoint expects
    payload = %{
      "public_inputs" => [],
      "program" => program,
      "private_inputs" => []
    }

    data =
      client.conn
      |> post(~p"/nock/prove", payload)
      |> json_response(200)

    expected_io = []

    expected_result = Base.encode64(Noun.Jam.jam(<<0>>))

    assert data == %{
             "io" => expected_io,
             "result" => expected_result
           }

    client
  end

  @doc """
  I add a transaction to the mempool.
  """
  @spec prove_nock_with_arg(EClient.t()) :: {EClient.t(), String.t()}
  def prove_nock_with_arg(client \\ setup()) do
    program = Base.encode64(squared())
    inputs = [Base.encode64(Noun.Jam.jam(3))]

    # the json payload the endpoint expects
    payload = %{
      "public_inputs" => inputs,
      "program" => program,
      "private_inputs" => []
    }

    data =
      client.conn
      |> post(~p"/nock/prove", payload)
      |> json_response(200)

    expected_io = []

    expected_result = Base.encode64(Noun.Jam.jam(<<9>>))

    assert data == %{
             "io" => expected_io,
             "result" => expected_result
           }

    client
  end

  @doc """
  I add a transaction to the mempool.
  """
  @spec prove_nock_with_io(EClient.t()) :: {EClient.t(), String.t()}
  def prove_nock_with_io(client \\ setup()) do
    program = Base.encode64(tracing())
    # the json payload the endpoint expects
    payload = %{
      "public_inputs" => [],
      "private_inputs" => [],
      "program" => program
    }

    data =
      client.conn
      |> post(~p"/nock/prove", payload)
      |> json_response(200)

    expected_io =
      [<<1>>, <<4>>, <<2>>, <<4>>]
      |> Enum.map(&Noun.Jam.jam/1)
      |> Enum.map(&Base.encode64/1)

    expected_result = Base.encode64(Noun.Jam.jam(<<0>>))

    assert data == %{
             "io" => expected_io,
             "result" => expected_result
           }

    client
  end

  ############################################################
  #                           Helpers                        #
  ############################################################
  @doc """
  A juvix program that squares its input.
  """
  @spec squared() :: binary()
  def squared() do
    :code.priv_dir(:anoma_client)
    |> Path.join("test_juvix/Squared.nockma")
    |> File.read!()
  end

  @doc """
  A Juvix program that outputs something in the IO.
  It prints 1, 4, 2, 4 to IO.
  """
  @spec tracing() :: binary()
  def tracing() do
    :code.priv_dir(:anoma_client)
    |> Path.join("test_juvix/Tracing.nockma")
    |> File.read!()
  end
end
