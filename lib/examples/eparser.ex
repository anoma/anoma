defmodule Examples.EParser do
  alias Anoma.Cli
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  @type parse_result() :: Optimus.ParseResult.t()

  @spec get_423(boolean()) ::
          {[Cli.client_commands(), ...], parse_result()}
  def get_423(silent \\ false) do
    flags =
      if silent do
        ["--silent"]
      else
        []
      end

    command_line = ["get", "423"] ++ flags

    assert {:ok, cmd, res} =
             Optimus.parse(Anoma.Cli.argument_parser(), command_line)

    assert cmd == [:get]
    assert %{key: 423} == res.args
    {cmd, res}
  end

  @type zero_submit_423() :: {[Cli.client_commands(), ...], parse_result()}
  def zero_submit_423() do
    zero_nock = "./test/data/zero-423.nock"
    submit_cmd(zero_nock, ro: false)
  end

  @type inc_submit_423() :: {[Cli.client_commands(), ...], parse_result()}
  def inc_submit_423() do
    inc_nock = "./test/data/inc-423.nock"
    submit_cmd(inc_nock, ro: false)
  end

  @spec get_ro_submit_423(boolean()) ::
          {[Cli.client_commands(), ...], parse_result()}
  def get_ro_submit_423(silent \\ false) do
    get_ro_nock = "./test/data/get-ro-423.nock"
    submit_cmd(get_ro_nock, ro: true, silent: silent)
  end

  @spec plus_one_ro_submit_423(boolean()) ::
          {[Cli.client_commands(), ...], parse_result()}
  def plus_one_ro_submit_423(silent \\ false) do
    plus_one_ro_nock = "./test/data/plus-one-ro-423.nock"
    submit_cmd(plus_one_ro_nock, ro: true, silent: silent)
  end

  ####################################################################
  ##                             Helpers                            ##
  ####################################################################

  @spec submit_cmd(String.t(), Keyword.t()) ::
          {[Cli.client_commands(), ...], parse_result()}
  defp submit_cmd(filename, options) do
    ro = Keyword.get(options, :ro, false)

    submit =
      if ro do
        "ro-submit"
      else
        "submit"
      end

    silent =
      if Keyword.get(options, :silent) do
        ["--silent"]
      else
        []
      end

    command_line = [submit, filename] ++ silent

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
