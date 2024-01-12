defmodule Nock.Cli do
  import Nock
  require Noun.Format

  @spec argument_parser() :: Optimus.t()
  def argument_parser() do
    Optimus.new!(
      name: "nockma",
      description: """
      Run the nockma evaluator on a file. \
      The file should contain a single nockma cell: [subject formula]
      """,
      allow_unknown_args: false,
      parse_double_dash: true,
      args: [
        infile: [
          value_name: "INPUT_FILE",
          help: "Nockma source file",
          required: true,
          parser: :string
        ]
      ],
      options: [
        outfile: [
          value_name: "OUTPUT_FILE",
          short: "-o",
          long: "--output",
          help: "A file to write the evaluation result",
          parser: :string,
          required: false,
          default: nil
        ]
      ]
    )
  end

  @spec main([binary()]) :: :ok
  def main(argv) do
    args = Optimus.parse!(argument_parser(), argv)
    %{args: %{infile: infile}, options: %{outfile: outfile}} = args
    outfile = outfile || infile <> ".result"
    {:ok, contents} = File.read(infile)
    {:ok, [subject | formula]} = Noun.Format.parse(contents)
    {:ok, res} = nock(subject, formula)
    :ok = File.write!(outfile, Noun.Format.print(res))
  end
end
