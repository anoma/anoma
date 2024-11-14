defmodule Livebook do
  @moduledoc """
  I generate out extra information for Livebook

  My main purpose is to generate out the TOC for each livebook
  document we have.

  to do this please run `toc_toplevel/0`

  To set a certain order please set `sort_order/0` to have the updated
  order

  ## API

  - `sort_order/0`
  - `toc_toplevel/0`
  - `get_all_livemd_documents/0`
  - `example_toc/0`

  """

  @type sort() :: list({String.t(), sort()})

  @sort_order "doc_order.exs" |> Code.eval_file() |> elem(0)
  @spec sort_order() :: sort()

  def sort_order() do
    @sort_order
  end

  ####################################################################
  ##                            Auto Run                             #
  ####################################################################

  @doc """
  I get out all live view docs
  """
  @spec get_all_livemd_documents() :: list(Path.t())
  def get_all_livemd_documents() do
    get_livemd_documents("./documentation")
  end

  @doc """
  I provide an example of what a TOC looks like
  """
  @spec example_toc() :: :ok
  def example_toc() do
    Livebook.get_all_livemd_documents()
    |> Enum.map(fn x -> String.replace_prefix(x, "documentation/", "") end)
    |> generate_toc()
    |> IO.puts()
  end

  @doc """
  I generate out the TOC for all liveview docs
  """
  @spec toc_toplevel() :: :ok
  def toc_toplevel() do
    paths = Livebook.get_all_livemd_documents()

    paths_without_doc =
      paths
      |> Enum.map(fn x -> String.replace_prefix(x, "documentation/", "") end)

    paths
    |> Enum.map(fn p ->
      {p,
       generate_toc(
         paths_without_doc,
         count_depth(String.replace_prefix(p, "documentation/", ""))
       )}
    end)
    |> Enum.each(fn {path, toc} ->
      inject_toc(toc, path)
    end)
  end

  ####################################################################
  ##                            Injection                            #
  ####################################################################

  @spec inject_toc(String.t(), Path.t()) :: :ok
  def inject_toc(toc, path) do
    data =
      File.read!(path)
      |> Livebook.change_header("##", "Index", toc)

    File.write!(path, data)
  end

  ####################################################################
  ##                        Getting Documents                        #
  ####################################################################

  @doc """
  Gets all livemd documents in a sorted list given a path.
  """
  @spec get_livemd_documents(Path.t()) :: list(Path.t())
  def get_livemd_documents(dir) do
    [dir | dir_from_path(dir)]
    |> Stream.map(fn x -> Path.wildcard(Path.join(x, "*livemd")) end)
    |> Stream.concat()
    |> Enum.sort_by(&insert_sort_order/1)
  end

  ####################################################################
  ##                            Generation                           #
  ####################################################################

  @doc """
  Generates out a TOC, given a series of nested documents

  We take a path, and a place where we should be calculating the TOC from.

  ## Example

  """
  @spec generate_toc(list(Path.t()), non_neg_integer()) :: String.t()
  @spec generate_toc(list(Path.t())) :: String.t()
  def generate_toc(documents, from_depth \\ 0) do
    documents
    |> Livebook.add_heading_num()
    |> Enum.map_join("\n", fn {f, d, n} ->
      generate_heading(f, d, n, from_depth)
    end)
  end

  @spec add_heading_num(list(String.t())) ::
          list({String.t(), non_neg_integer(), non_neg_integer()})
  def add_heading_num(documents) do
    documents
    |> Stream.map(fn x -> {x, count_depth(x)} end)
    # FOLDS ARE BAD
    # We simply have a stack for checking to resume numbering after
    # going to a sister after nesting
    # further the number of spacing is variable and so is also counted
    # with the stack
    |> Enum.reduce({[], 0, 0, 0, []}, fn {file, depth_f},
                                         {list, depth_prev, last, num_spaces,
                                          stack} ->
      up_n = depth_prev - depth_f
      # If the previous depth agrees, increment the numbers
      cond do
        # The previous filing was our sister
        depth_f == depth_prev ->
          {[{file, num_spaces, last + 1} | list], depth_f, last + 1,
           num_spaces, stack}

        # The previous file is a parent
        depth_f > depth_prev ->
          new_spacing = num_spaces + 2 + String.length(to_string(last))

          {[{file, new_spacing, 1} | list], depth_f, 1, new_spacing,
           [{last, num_spaces} | stack]}

        # The previous file is a sister of one of our ancestors
        depth_f < depth_prev ->
          {last_numbering, last_spaces} = Enum.at(stack, up_n - 1)
          new_numbering = last_numbering + 1

          {[{file, last_spaces, new_numbering} | list], depth_f,
           new_numbering, last_spaces, Enum.drop(stack, up_n)}
      end
    end)
    |> elem(0)
    |> Enum.reverse()
  end

  @spec generate_heading(String.t(), non_neg_integer(), non_neg_integer()) ::
          String.t()
  @spec generate_heading(
          String.t(),
          non_neg_integer(),
          non_neg_integer(),
          non_neg_integer()
        ) :: String.t()
  def generate_heading(path, depth, numbering, from_depth \\ 1) do
    String.duplicate(" ", depth) <>
      to_string(numbering) <>
      ". " <>
      "[#{name(path)}](#{relative(path, from_depth)})"
  end

  @spec dir_from_path(String.t()) :: list(String.t())
  def dir_from_path(dir) do
    File.ls!(dir)
    |> Stream.map(&Path.join(dir, &1))
    |> Stream.filter(&File.dir?(&1))
    |> Enum.map(fn x -> [x | dir_from_path(x)] end)
    |> Enum.concat()
  end

  ####################################################################
  ##                         Custom Sorting                          #
  ####################################################################
  @spec sort_value(maybe_improper_list()) :: list(number())
  defp sort_value(document) do
    sort_value(document, sort_order(), [])
  end

  defp sort_value([], _sort, starting) do
    Enum.reverse([0 | starting])
  end

  defp sort_value([path | rest], sort, starting) do
    index =
      Enum.find_index(sort, fn {sort_doc, _} ->
        path == sort_doc
      end)

    if index do
      sort_value(rest, Enum.at(sort, index) |> elem(1), [index | starting])
    else
      Enum.reverse([2 ** 32 | starting])
    end
  end

  @spec insert_sort_order(String.t()) :: {list(number()), String.t()}
  defp insert_sort_order(str) do
    {sort_value(Path.rootname(str) |> Path.split()), str}
  end

  ####################################################################
  ##                           Name Help                             #
  ####################################################################
  @spec name(String.t()) :: String.t()
  defp name(path) do
    Path.basename(path, ".livemd")
    |> String.split("-")
    |> Stream.map(&String.capitalize(&1))
    |> Enum.join(" ")
  end

  @spec relative(Path.t(), non_neg_integer) :: Path.t()
  defp relative(path, relative) do
    "./" <>
      String.duplicate("../", relative) <>
      path
  end

  @spec count_depth(Path.t()) :: non_neg_integer()
  def count_depth(path) do
    path
    |> String.graphemes()
    |> Enum.count(&(&1 == "/"))
  end

  @doc """
  I replace the header with the given TOC

  ### Example
    > markdown_text = "## Intro ... \n## Index \n text here \n ## Conclusion \n All good"
    > Livebook.change_header(markdown_text, "##", "Index", "New Content") |> IO.puts
       ## Intro ...
       ## Index
       New Content
       ## Conclusion
       All good
      :ok
  """
  @spec change_header(String.t(), String.t(), String.t(), String.t()) ::
          String.t()
  def change_header(markdown, header_level, start_header, new_text) do
    header_regex =
      ~r/^#{header_level}\s+#{start_header}\s*$(.*?)(?=\n#{header_level}\s+|##|\z)/ms

    updated_markdown =
      String.replace(
        markdown,
        header_regex,
        "#{header_level} #{start_header}\n#{new_text}\n"
      )

    updated_markdown
  end
end
