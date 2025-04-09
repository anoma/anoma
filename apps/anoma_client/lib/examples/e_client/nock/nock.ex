defmodule Anoma.Client.Examples.EClient.Nock do
  @moduledoc """
  I contain functions to test the public interface of the client.

  I show how to use the nock run and prove endpoints.
  """
  use Anoma.Client.Web.ConnCase

  ############################################################
  #                           Example Programs               #
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
