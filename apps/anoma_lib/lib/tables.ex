defmodule Anoma.Node.Tables do
  require Logger

  ############################################################
  #                    Types                                 #
  ############################################################

  @typedoc """
  A table is typed by a tuple where the first element is
  the table name, and the second one is a list of keys (i.e., columns).
  """
  @type table_spec :: {atom(), list(atom())}

  @typedoc "a list of table specs"
  @type table_specs :: list(table_spec)

  ############################################################
  #                    Public                                #
  ############################################################

  @tables [
    {Events, [:type, :body]},
    {CommitmentTree, [:index, :hash]},
    {Blocks, [:round, :block]},
    {Values, [:key, :value]},
    {Updates, [:key, :value]},
    {Intents, [:type, :body]}
  ]

  @doc """
  I return the table name for the intents table.
  """
  @spec table_intents(String.t()) :: atom()
  def table_intents(node_id) do
    node_table_name(node_id, Intents)
  end

  @doc """
  I return the table name for the given node its event table.
  """
  @spec table_events(String.t()) :: atom()
  def table_events(node_id) do
    node_table_name(node_id, Events)
  end

  @doc """
  I return the table name for the given node its commitment tree.
  I can be called without a node id too. In that case I use "no_node" as node id.
  """
  @spec table_commitment_tree(String.t()) :: atom()
  def table_commitment_tree(node_id \\ "no_node") do
    node_table_name(node_id, CommitmentTree)
  end

  @doc """
  I return the table name for the given node its blocks.
  """
  @spec table_blocks(String.t()) :: atom()
  def table_blocks(node_id) do
    node_table_name(node_id, Blocks)
  end

  @doc """
  I return the table name for the given node its values.
  """
  @spec table_values(String.t()) :: atom()
  def table_values(node_id) do
    node_table_name(node_id, Values)
  end

  @doc """
  I return the table name for the given node its updates.
  """
  @spec table_updates(String.t()) :: atom()
  def table_updates(node_id) do
    node_table_name(node_id, Values)
  end

  @doc """
  I initialize the tables for a given node id.
  I do this by creating all tables in the mnesia storage.

  I return information whether these tables did not exist, were created,
  or when I failed to create them.
  """
  @spec initialize_tables_for_node(String.t()) ::
          {:ok, :existing | :created} | {:error, :failed_to_initialize_tables}
  def initialize_tables_for_node(node_id) do
    @tables
    |> Enum.map(fn {table, fields} ->
      {node_table_name(node_id, table), fields}
    end)
    |> create_tables()
    |> case do
      {:error, :failed_to_create_table, _table, _fields, _err} ->
        {:error, :failed_to_initialize_tables}

      {:ok, {created, existing}} ->
        case :mnesia.wait_for_tables(created ++ existing, 60_000) do
          :ok ->
            case {created, existing} do
              {_, []} ->
                {:ok, :created}

              _ ->
                {:ok, :existing}
            end

          {:timeout, error} ->
            Logger.error(error)
            {:error, :failed_to_initialize_tables}
        end
    end
  end

  @doc """
  I initialize the mnesia storage for this entire vm.
  """
  @spec initialize_storage() ::
          :ok
          | {:error, :failed_to_initialize_storage}
          | {:error, :failed_to_create_tables}
  def initialize_storage() do
    configure_mnesia()

    with :ok <- create_local_schema(),
         :ok <- :mnesia.start(),
         :ok <- init_rocksdb() do
      :ok
    else
      {:error, :failed_to_create_schema, _error} ->
        {:error, :failed_to_create_schema}

      {:error, :failed_to_initialize_rocksdb} ->
        {:error, :failed_to_initialize_rocksdb}

      _error ->
        {:error, :failed_to_initialize_storage}
    end
  end

  @doc """
  Given an atom as table name, I create a node-specific name based on that.
  """
  @spec node_table_name(String.t(), atom()) :: atom()
  def node_table_name(node_id, name) do
    String.to_atom("#{node_id}_#{name}")
  end

  @doc """
  I duplicate a given table's content to a new table.
  If the new table does not exist, it is created.
  """
  @spec duplicate_table(atom(), atom()) ::
          {:ok, :table_copied} | {:error, :copy_failed, any()}
  def duplicate_table(source_table, target_table) do
    # get the attributes (i.e., columns) of the source table
    table_attributes = :mnesia.table_info(source_table, :attributes)

    # create a copy of the table
    create_table(target_table, table_attributes)

    :mnesia.transaction(fn ->
      copy_table_rows(source_table, target_table)
    end)
    |> case do
      {:atomic, :ok} ->
        {:ok, :table_copied}

      e ->
        {:error, :copy_failed, e}
    end
  end

  @doc """
  I clear out the given table.
  """
  @spec clear_table(atom()) :: :ok | {:error, :failed_to_clear_table}
  def clear_table(table_name) do
    case :mnesia.clear_table(table_name) do
      {:atomic, _} ->
        :ok

      _ ->
        {:error, :failed_to_clear_table}
    end
  end

  @spec has_data?(String.t()) ::
          {:ok, :exists}
          | {:error, :partial_exist | :none_exist | :failed_to_check_tables}
  def has_data?(node_id) do
    @tables
    |> Enum.map(fn {table, _fields} ->
      node_table_name(node_id, table)
    end)
    |> tables_exist?()
  end

  ############################################################
  #                  Private Helpers                         #
  ############################################################

  @doc """
  Check if a given list of tables exists.

  This function takes a list of tables to avoid waiting for tables
  multiple times.
  """
  @spec tables_exist?([atom()]) ::
          {:ok, :exists}
          | {:error, :partial_exist | :none_exist | :failed_to_check_tables}
  def tables_exist?(table_names) do
    :mnesia.transaction(fn ->
      table_names
      |> Enum.reject(&table_exists?/1)
      |> case do
        [] ->
          {:ok, :exists}

        non_existing ->
          if Enum.count(non_existing) == Enum.count(table_names) do
            {:error, :none_exist}
          else
            {:error, :partial_exist}
          end
      end
    end)
    |> case do
      {:atomic, result} ->
        result

      e ->
        Logger.error(inspect(e))
        {:error, :failed_to_check_tables}
    end
  end

  @doc """
  Check if the given table name exists.

  Run inside transaction.
  """
  @spec table_exists?(atom()) :: boolean
  def table_exists?(table) do
    try do
      # this call raises if the table does not exist.
      :mnesia.table_info(table, :all)
      true
    catch
      :exit, {:aborted, {:no_exists, ^table, :all}} ->
        false
    end
  end

  # @doc """
  # I create a table with the given name, scoped to a specific node.
  # """
  @spec create_table(atom(), list(atom())) ::
          {:ok, :exists | :created} | {:error, :failed_to_create_table, any()}
  def create_table(name, fields) do
    # determine whether to use rocksdb options or not
    table_opts =
      [attributes: fields] ++
        if config()[:rocksdb], do: [rocksdb_copies: [node()]], else: []

    case :mnesia.create_table(name, table_opts) do
      {:aborted, {:already_exists, _}} ->
        {:ok, :exists}

      {:atomic, :ok} ->
        {:ok, :created}

      err ->
        {:error, :failed_to_create_table, err}
    end
  end

  # @doc """
  # I create all the tables given to me for the specific node.
  # I return a list of created tables, and a list of tables that already existed.
  # """
  @spec create_tables(list({atom(), list(atom())})) ::
          {:ok, {[atom()], [atom()]}}
          | {:error, :failed_to_create_table, atom(), list(atom()), any()}
  def create_tables(table_list) do
    table_list
    |> Enum.reduce_while({[], []}, fn {table, fields}, {new, existing} ->
      case create_table(table, fields) do
        {:ok, :created} ->
          {:cont, {[table | new], existing}}

        {:ok, :exists} ->
          {:cont, {new, [table | existing]}}

        {:error, :failed_to_create_table, err} ->
          {:halt, {:error, :failed_to_create_table, table, fields, err}}
      end
    end)
    |> case do
      {_, _} = tables ->
        {:ok, tables}

      e ->
        e
    end
  end

  # @doc """
  # I create the schema for the mnesia table.
  # If the schema exists, I also return `:ok`.
  # """
  @spec create_local_schema() ::
          :ok | {:error, :failed_to_create_schema, any()}
  def create_local_schema() do
    # only nodes with disks can create schemas
    if Application.get_env(:mnesia, :schema_location) != :ram do
      case :mnesia.create_schema([node()]) do
        :ok ->
          :ok

        {:error, {_, {:already_exists, _node}}} ->
          :ok

        {:error, err} ->
          {:error, :failed_to_create_schema, err}
      end
    else
      :ok
    end
  end

  # -----------------------------------------------------------
  # Copy tables

  # @doc """
  # I copy all records from the source table to the target table given a continuation.
  # """
  @spec copy_table_rows(any(), any(), any()) :: :ok
  def copy_table_rows(source_table, target_table, cont \\ nil)

  def copy_table_rows(source_table, target_table, nil) do
    # get the wildcard to match all entries in the table
    wild_pattern = :mnesia.table_info(source_table, :wild_pattern)

    # read the chunks from the table
    # :mnesia.select(source_table, [{wild_pattern, [], [~c"$_"]}], 10, :read)
    result =
      :mnesia.select(source_table, [{wild_pattern, [], [:"$_"]}], 10, :read)

    case result do
      {items, cont} ->
        # write the chunks to the new table
        for item <- items do
          item
          # update the record to new table
          |> put_elem(0, target_table)
          |> :mnesia.write()
        end

        # move all rows from source to target
        copy_table_rows(source_table, target_table, cont)

      # there were no items in the table
      :"$end_of_table" ->
        :ok
    end
  end

  def copy_table_rows(_, _, :"$end_of_table") do
    :ok
  end

  def copy_table_rows(source_table, target_table, cont) do
    case :mnesia.select(cont) do
      {items, cont} ->
        # write the chunks to the new table
        for item <- items do
          :mnesia.write(put_elem(item, 0, target_table))
        end

        copy_table_rows(source_table, target_table, cont)

      # no items left
      _ ->
        :ok
    end
  end

  # -----------------------------------------------------------
  # Mnesia config

  # @doc """
  # I initialize rocksdb.
  # If the configuration does not require rocksdb, I do nothing.
  # """
  @spec init_rocksdb :: :ok | {:error, :failed_to_initialize_rocksdb}
  defp init_rocksdb() do
    persist? = config()[:persist_to_disk]

    rocksdb? = config()[:rocksdb]

    if persist? and rocksdb? do
      case :mnesia_rocksdb.register() do
        {:ok, :rocksdb_copies} ->
          :ok

        _ ->
          {:error, :failed_to_initialize_rocksdb}
      end
    else
      :ok
    end
  end

  # @doc """
  # I put the mnesia configuration parameters based on the config file.
  # """
  @spec configure_mnesia :: :ok
  defp configure_mnesia do
    # disable writing to disk if set
    if config()[:persist_to_disk] do
      Application.put_env(:mnesia, :schema_location, :opt_disc)
    else
      Application.put_env(:mnesia, :schema_location, :ram)
    end

    # set default data directory
    # if the option is unset, a default dir is chosen
    Application.put_env(
      :mnesia,
      :dir,
      String.to_charlist(config()[:data_dir])
    )
  end

  # @doc """
  # I return the directory where to store the mnesia data by default.
  # """
  @spec mnesia_data_dir :: String.t()
  defp mnesia_data_dir() do
    mnesia_data_dir(:os.type())
  end

  defp mnesia_data_dir({:unix, :darwin}) do
    Path.expand("~/Library/Application Support/Anoma")
  end

  defp mnesia_data_dir({:unix, :linux}) do
    case System.get_env("XDG_DATA_HOME") do
      nil ->
        Path.expand("~/.config/anoma")

      dir ->
        Path.join(dir, "anoma")
    end
  end

  # @doc """
  # I return the configuration parameters for mnesia.
  # I also prefill the default values.
  # Do not read Application.get_env by hand!
  # """
  defp config() do
    config =
      Application.get_env(:anoma_node, :mnesia, [])
      |> Keyword.validate!(
        data_dir: mnesia_data_dir(),
        rocksdb: true,
        persist_to_disk: true
      )

    if config[:rocksdb] and not config[:persist_to_disk] do
      Logger.warning(
        "rocksdb is enabled, but persistence to disk is off. rocksdb will always write data to disk."
      )
    end

    config
  end
end
