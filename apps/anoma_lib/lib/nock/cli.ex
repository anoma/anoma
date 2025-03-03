defmodule Nock.Cli do
  require Noun.Format

  import Nock

  @spec argument_option() :: list()
  def argument_option() do
    [
      name: "nockma",
      about: """
      Run the nockma evaluator on a file. \
      The file should contain a single nockma cell: [subject formula]
      """,
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
    ]
  end

  @spec main(term()) :: term()
  def main(%{args: %{infile: infile}, options: %{outfile: outfile}}) do
    outfile = outfile || infile <> ".result"
    {:ok, contents} = File.read(infile)
    {:ok, [subject | formula]} = Noun.Format.parse(contents)
    {:ok, res} = nock(subject, formula)
    :ok = File.write!(outfile, Noun.Format.print(res))
  end
end
