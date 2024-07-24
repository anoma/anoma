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
    zero_nock = "./test/data/zero-423.nock"
    submit_cmd(zero_nock, false)
  end

  @type inc_submit_423() :: {[Cli.client_commands(), ...], parse_result()}
  def inc_submit_423() do
    inc_nock = "./test/data/inc-423.nock"
    submit_cmd(inc_nock, false)
  end

  ####################################################################
  ##                             Helpers                            ##
  ####################################################################

  @spec submit_cmd(String.t(), boolean()) ::
          {[Cli.client_commands(), ...], parse_result()}
  defp submit_cmd(filename, ro) do
    submit =
      if ro do
        "ro-submit"
      else
        "submit"
      end

    command_line = [submit, filename]

    assert {:ok, cmd, res} =
             Optimus.parse(Anoma.Cli.argument_parser(), command_line)

    if ro do
      assert [:ro_submit] == cmd
    else
      assert [:submit] == cmd
    end

    assert %{file: filename} == res.args
    {cmd, res}
  end
end
