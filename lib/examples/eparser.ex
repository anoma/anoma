defmodule Examples.EParser do
  alias Anoma.Cli
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  @type parse_result() :: Optimus.ParseResult.t()

  @type get_423() :: {[Cli.client_commands(), ...], parse_result()}
  def get_423() do
    seeded_argument = ["get", "423"]

    assert {:ok, cmd, res} =
             Optimus.parse(Anoma.Cli.argument_parser(), seeded_argument)

    assert cmd == [:get]
    assert %{key: 423} == res.args
    {cmd, res}
  end

  @type zero_submit_423() :: {[Cli.client_commands(), ...], parse_result()}
  def zero_submit_423() do
    file = "./test/data/zero-423.nock"
    seeded_argument = ["submit", file]

    assert {:ok, cmd, res} =
             Optimus.parse(Anoma.Cli.argument_parser(), seeded_argument)

    assert [:submit] == cmd
    assert %{file: file} == res.args
    {cmd, res}
  end
end
