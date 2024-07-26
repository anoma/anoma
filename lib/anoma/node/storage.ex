defmodule Anoma.Node.Storage do
  @moduledoc """
  I am the Anoma Storage Engine.

  The Anoma database usage is separated into two parts:

  1) The Ordering table provides info on the order of appropriate keys.
     That is, it has info about how many times the key has been assigned to
     some value.

  2) The Qualified table provides a representation of all stored key-values
     in a particular format. Each entry has inside it

     1) The table name attached
     2) The order of the key given by the Ordering
     3) The namespace idenifying the Storage Engine
     4) The key itself as provided by the user.

  As a Storage Engine, I provide the appropriate functionality
  to coordinate the Ordering and Qualified tables of a launched Anoma Node.

  That is, I help with writing things respecting the ordering of events
  while providing appropriate reading capabilities of the tables used, so
  that the base read events read only the latest value of a given key.

  ### Public API

  I provide the following public functionality:

  #### Setup

  - `setup/1`
  - `remove/1`
  - `do_setup/1`
  - `ensure_new/2`
  - `do_ensure_new/2`

  #### Writing

  - `put_snapshot/2`
  - `delete_key/2`
  - `put/3`

  #### Reading

  - `snapshot_order/1`
  - `get/2`
  - `get_keyspace/2`
  - `get_at_snapshot/2`
  - `in_snapshot/2`
  - `read_order/2`
  - `read_order_tx/2`
  - `read_at_order/3`
  - `read_at_order_tx/3`


  #### Blocking

  - `blocking_read/2`

  #### Other

  - `cm_tree_spec/0`
  """

  alias Anoma.Node.{Router, Logger}
  alias __MODULE__

  use TypedStruct
  use Router.Engine

  @typedoc """
  I am the type of the Storage Engine.

  I store info on the db tables used by the Node and info for appropriate
  read/write actions.

  ### Fields

    - `:qualified` - The name of the Qualified table.
                     Default: Anoma.Node.Storage.Qualified
    - `:order` - The name of the Ordering table.
                 Default: Anoma.Node.Storage.Order
    - `:rm_commitments` - The name of the Reource Machine Commitments table.
                          Default: Anoma.Node.Storage.RMCommitments
    - `:namespace` - The namespace used for writing functionality.
                     Default: []
    - `:topic` - The subscription topic.
                 Enforced: false
  """
  typedstruct do
    field(:qualified, atom(), default: Anoma.Node.Storage.Qualified)
    field(:order, atom(), default: Anoma.Node.Storage.Order)
    field(:rm_commitments, atom(), default: Anoma.Node.Storage.RMCommitments)
    field(:namespace, list(binary()), default: [])
    field(:topic, Router.Addr.t(), enforce: false)
  end

  @typedoc """
  I am the type of a mensia table query result.
  """
  @type result(res) :: {:atomic, res} | {:aborted, any()}

  @typedoc """
  I designate the type of keys for Ordering table queries.
  """
  @type order_key() :: Noun.t()

  @typedoc """
  I designate the type of values of keys for Ordering table queries.
  """
  @type order_value() :: list({any(), Noun.t(), non_neg_integer()})

  @typedoc """
  I designate the type of keys for Qualified table queries.
  """
  @type qualified_key() :: nonempty_improper_list(any(), non_neg_integer())

  @typedoc """
  I designate the type of values of Keys for Qualified table queries.
  """
  @type qualified_value() :: any()

  @typedoc """
  I designate the snapshot type for table queries.
  """
  @type snapshot() :: {t(), list({order_key(), non_neg_integer()})}

  @doc """
  I am the initialization function for a Storage Engine Instance.

  ### Pattern-Macthing Variations

  - `init(%Storage{})` - I initialize the Engine with the given state.
  - `init(args)` - I expect a keylist/map with all the keywords matching
                   the Storage Engine type fields. I then setup the tables
                   and return the appropriate Engine state for startup.
  """

  @spec init(Storage.t()) :: {:ok, Storage.t()}
  def init(%__MODULE__{} = state) do
    {:ok, state}
  end

  @spec init(
          list(
            {:qualified, atom}
            | {:order, atom()}
            | {:rm_commitments, atom()}
            | {:namespace, list(binary())}
            | {:topic, Router.Addr.t()}
          )
          | %{
              qualified: atom(),
              order: atom(),
              rm_commitments: atom(),
              namespace: list(binary()),
              topic: Router.Addr.t()
            }
        ) :: {:ok, Storage.t()}
  def init(opts) do
    return = %Storage{}

    return = %Storage{
      qualified: opts[:qualified] || return.qualified,
      order: opts[:order] || return.order,
      rm_commitments: opts[:rm_commitments] || return.rm_commitments,
      namespace: opts[:namespace] || return.namespace,
      topic: opts[:topic] || return.namespace
    }

    # idempotent
    do_setup(return)
    {:ok, return}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am the main reading function for the Storage functionality.

  Given a storage address and key name in the Order format, I look up the
  latest value of such a key in the Qualified storage and return it in the
  standard mnesia format. Otherwise, I return `:absent` if no value is
  present (or if the key is tombstoned* in the latest instance).

  See `delete_key/2`
  """

  @spec get(Router.Addr.t(), order_key()) ::
          :absent | {:ok, qualified_value()} | {:error, :timed_out}
  def get(storage, key) do
    Router.call(storage, {:get, key})
  end

  @doc """
  I am the keyspace reading function for the Storage Engine.

  Given a storage address and a list of keys in the appropriate format, I
  first look up whether the keys appear in any qualified key in the
  Qualified table. If so, look up all apropriate latest values of all such
  keys and return them as a list of table query responses in a standard
  mnesia format. If not present, return `:absent`
  """

  @spec get_keyspace(Router.Addr.t(), list(order_key())) ::
          :absent
          | list({list(), qualified_value()})
          | {:atomic, any()}
  def get_keyspace(storage, key_space) do
    Router.call(storage, {:get_keyspace, key_space})
  end

  @doc """
  I provide a snapshotting functionality for the Ordering table.

  Given a Storage address, I look up the Ordering table attached to the
  Engine and provide a snapshot of it as a list of keys with their orders
  returned in a mnesia result format and the attached storage structure.
  """

  @spec snapshot_order(Router.Addr.t()) :: result(snapshot())
  def snapshot_order(storage) do
    Router.call(storage, :snapshot_order)
  end

  @doc """
  I am the main function for getting orders of keys.

  Given a Storage address and a key, I return the key with its specified
  order in the Ordering table Storage using `:mnesia.transaction` to get
  the value in standard mnesia format.

  See `read_order/2` for the exact mechanism behind order-reading.
  """

  @spec read_order_tx(Router.Addr.t(), order_key()) :: result(order_value())
  def read_order_tx(storage, key) do
    Router.call(storage, {:read_order_tx, key})
  end

  @doc """
  I am the function getting the value of a key at a particular order.

  My main role is to get transactions at the order given by the Ordering
  Engine. Generally, the user is to inquire about the value of a key using
  `get/2` functionality.

  Given a Storage address, a key, and the order, I return the list
  containing the name of the storage, the key as given in the inputs, and
  its value at the specified order in the traditional mnesia format using
  `:mnesia.transaction` capabilities.

  See `read_at_order/3` for how we actually read the value from the
  Qualified table.
  """

  @spec read_at_order_tx(Router.Addr.t(), Noun.t(), non_neg_integer()) ::
          result(list({atom(), qualified_key(), qualified_value()}))
  def read_at_order_tx(storage, key, order) do
    Router.call(storage, {:read_at_order_tx, key, order})
  end

  @doc """
  I am the storage removal function.

  Given a Storage Engine address, I ask mnesia to delete all tables found
  inside said Engine structure.
  """

  @spec remove(Router.Addr.t()) :: :ok
  def remove(storage) do
    Router.cast(storage, :remove)
  end

  @doc """
  I am the function ensuring appropriate db functionality.

  Given a Storage address and a boolean flag, I first use `remove/1`
  functionality to delete specified tables of the Engine and then
  ask for a setup using `setup/1` functionality with an appropriate flag
  for rocksdb copies.
  """

  @spec ensure_new(Router.Addr.t(), boolean()) :: :ok
  def ensure_new(storage, rocks \\ false) do
    Router.cast(storage, {:ensure_new, rocks})
  end

  @doc """
  I am the setup function for Storage.

  Given a Storage address, I look for the tables inside its structure,
  and then try to create all mnesia tables with the given names. On
  success, I also create a new commitment tree.
  """

  @spec setup(Router.Addr.t()) :: :ok
  def setup(t) do
    Router.cast(t, :setup)
  end

  @doc """
  I provide the main writing functionality of the Storage Engine.

  I am responsible for assigning the given values to key while respecting
  the underlying table structure.

  That is, given a key and a value, I first namespace the key getting the
  namespace info from the appropriate storage state. Secondly, I re-write
  the order-value assigned to said namespaced key, incrementing its value.
  Finally, I write the value given to me to the Qualified table, assigning
  it to the namspaced key with appropriate new order attached,
  corresponding to the one in the Ordering table.
  """

  @spec put(Router.Addr.t(), order_key(), qualified_value()) :: :ok
  def put(storage, key, value) do
    Router.cast(storage, {:put, key, value})
  end

  @doc """
  I am the function responsible for writing an Ordering snapshot to our db.

  Given a Storage Address and a key, I first snapshot the appropriate
  Ordering table - similarly to `snapshot_order/1` - and then put it using
  the functionality underlying `put/3` as a value for the supplied key.

  Consult `put/3` on the exact mechanism behind our writing.
  """

  @spec put_snapshot(Router.addr(), order_key()) :: :ok
  def put_snapshot(storage, key) do
    Router.cast(storage, {:put_snapshot, key})
  end

  @doc """
  I provide the main tombstoning functionality for the Storage Engine.

  My main goal is to provide the API to make a key indistinguishable from
  being absent in the underlying table. That is, I use `put/3` to make the
  latest key value `:absent`. This way, I make it impossible to distinguish
  between the given key never been put into the Qualified table and being
  deleted using the main `get/2` functionality.
  """

  @spec delete_key(Router.Addr.t(), order_key()) :: :ok
  def delete_key(storage, key) do
    Router.cast(storage, {:delete, key})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call({:get, key}, _, storage) do
    {:reply, do_get(storage, key), storage}
  end

  def handle_call({:get_keyspace, key_space}, _, storage) do
    {:reply, do_get_keyspace(storage, key_space), storage}
  end

  def handle_call({:read_order_tx, key}, _, storage) do
    {:reply, do_read_order_tx(storage, key), storage}
  end

  def handle_call(:snapshot_order, _, storage) do
    {:reply, do_snapshot_order(storage), storage}
  end

  def handle_call({:read_at_order_tx, key, order}, _, storage) do
    {:reply, do_read_at_order_tx(storage, key, order), storage}
  end

  def handle_cast(:remove, _, storage) do
    do_remove(storage)
    {:noreply, storage}
  end

  def handle_cast({:ensure_new, rocks}, _, storage) do
    do_ensure_new(storage, rocks)
    {:noreply, storage}
  end

  def handle_cast(:setup, _, t) do
    do_setup(t)
    {:noreply, t}
  end

  def handle_cast({:put, key, value}, _, storage) do
    do_put(storage, key, value)
    {:noreply, storage}
  end

  def handle_cast({:put_snapshot, key}, _, storage) do
    do_put_snapshot(storage, key)
    {:noreply, storage}
  end

  def handle_cast({:delete, key}, _, storage) do
    do_delete_key(storage, key)
    {:noreply, storage}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @doc """
  I ensure that the fed in storage names correspond to completely empty new
  tables.

  I first delete all tables as specified in the Storage Engine structure,
  afterwards creating the tables with given names using `do_setup/2`.
  """

  @spec do_ensure_new(t()) :: :ok | {:aborted, any()}
  @spec do_ensure_new(t(), boolean()) :: :ok | {:aborted, any()}
  def do_ensure_new(storage = %__MODULE__{}, rocks \\ false) do
    # We don't care about errors that can happen here
    do_remove(storage)
    do_setup(storage, rocks)
  end

  @spec do_remove(t()) :: :ok
  defp do_remove(storage = %__MODULE__{}) do
    _topic = storage.topic
    _del_q = :mnesia.delete_table(storage.qualified)
    _del_o = :mnesia.delete_table(storage.order)
    _del_c = :mnesia.delete_table(storage.rm_commitments)

    # unless topic == nil do
    #  [
    #    {:delete_qualified, del_q},
    #    {:delete_ordering, del_o},
    #    {:delete_commitments, del_c}
    #  ]
    #  |> Enum.map(fn x -> Router.cast(topic, x) end)
    # end

    :ok
  end

  @doc """
  I setup storage with the given tables from the Storage Engine structure.

  I will try to create all values of storage, even if the first one fails
  due to already being created, we will try the others, setting them using
  rocksdb depending on the second argument given in the argument.
  """

  @spec do_setup(t()) :: :ok | {:aborted, any()}
  @spec do_setup(t(), boolean()) :: :ok | {:aborted, any()}
  def do_setup(t = %__MODULE__{}, rocks \\ false) do
    add_rocks = fn attrs ->
      if rocks do
        [{:rocksdb_copies, [node()]} | attrs]
      else
        attrs
      end
    end

    with {:atomic, :ok} <-
           :mnesia.create_table(
             t.qualified,
             add_rocks.(attributes: [:key, :value])
           ),
         {:atomic, :ok} <-
           :mnesia.create_table(
             t.order,
             add_rocks.(attributes: [:key, :order])
           ),
         {:atomic, :ok} <-
           :mnesia.create_table(
             t.rm_commitments,
             add_rocks.(attributes: [:index, :hash])
           ) do
      CommitmentTree.new(CommitmentTree.Spec.cm_tree_spec(), t.rm_commitments)
    else
      # It is a question as to if we should reset the storage
      # schema... Instead we ask the user to do it if we notice the
      # issue
      {:aborted, {:already_exists, table}} ->
        unless does_storage_actually_exist(table) do
          log_info({:restarting_storage, table})
        end

        {:aborted, {:already_exists, table}}

      error ->
        error
    end
  end

  @spec do_get(t(), order_key()) :: :absent | {:ok, qualified_value()}
  defp do_get(storage = %__MODULE__{}, key) do
    with {:atomic, [{_, ^key, order}]} <- do_read_order_tx(storage, key) do
      checked_read_at(storage, key, order)
    else
      _ -> :absent
    end
  end

  @spec do_put_snapshot(t(), order_key()) :: :ok | nil
  defp do_put_snapshot(storage = %__MODULE__{}, key) do
    with {:atomic, snapshot} <- do_snapshot_order(storage) do
      do_put(storage, key, snapshot)
    end
  end

  @spec do_delete_key(t(), order_key()) :: :ok | nil
  defp do_delete_key(storage = %__MODULE__{}, key) do
    log_info({:delete_key, key})
    do_put(storage, key, :absent)
  end

  @spec do_put(t(), order_key(), qualified_value()) :: :ok | nil
  defp do_put(storage = %__MODULE__{}, key, value) do
    tx = fn ->
      order = read_order(storage, key)
      new_order = calculate_order(order)
      write_at_order(storage, key, value, new_order)
      log_info({:put_order, new_order})
    end

    topic = storage.topic
    mtx = :mnesia.transaction(tx)
    msg = {:put, key, value} |> Tuple.append(mtx)

    unless topic == nil do
      storage.topic |> Router.cast(msg)
    end
  end

  @spec do_read_order_tx(t(), order_key()) ::
          result(list({atom(), Noun.t(), non_neg_integer()}))
  defp do_read_order_tx(storage = %__MODULE__{}, key) do
    tx = fn -> read_order(storage, key) end
    :mnesia.transaction(tx)
  end

  @spec do_get_keyspace(Storage.t(), list(any())) ::
          []
          | list({list(), qualified_value()})
          | {:atomic, any()}
  defp do_get_keyspace(storage = %__MODULE__{}, key_space) do
    with {:atomic, orders} <- read_keyspace_order(storage, key_space) do
      Enum.reduce(orders, [], fn [key, order], acc ->
        case checked_read_at(storage, key, order) do
          {:ok, val} -> [{key, val} | acc]
          :absent -> acc
        end
      end)
    end
  end

  @spec do_read_at_order_tx(t(), Noun.t(), non_neg_integer()) ::
          result(list({atom(), qualified_key(), qualified_value()}))
  defp do_read_at_order_tx(storage = %__MODULE__{}, key, order) do
    tx = fn -> read_at_order(storage, key, order) end
    :mnesia.transaction(tx)
  end

  @spec do_snapshot_order(t()) :: result(snapshot())
  defp do_snapshot_order(storage = %__MODULE__{}) do
    :mnesia.transaction(fn ->
      snapshot = [{{:"$1", :"$2", :"$3"}, [], [{{:"$2", :"$3"}}]}]
      {storage, :mnesia.select(storage.order, snapshot)}
    end)
  end

  ############################################################
  #                        Blocking                          #
  ############################################################

  @doc """
  I provide the final part of the blocking functionality for the Worker
  necessary for transaction-execution.

  ### General Usage

  I expect a Storage Engine address alingside with a key. Given a key which
  isn't a list, I error. Given a key which is a list starting with 0, I
  error following Nock and Ordering semantics. Given any other list, I
  subscribe to a Qualified table that is attached to the given Storage and
  read it in the Qualified table.

  If the key has a non-empty value in the table, I return `{:ok, value}`
  and unsubscribe from the table.

  If the key does not have a value in the table, I wait until I receive a
  `:write` message from the table, where the key written is exactlythe key
  supplied. Given the message, I return the written value as `{:ok, value}`
  and unsubscribe from the table.

  ### Anoma-Intended Semantics

  My intended use in the Anoma Lifecycle is as follows:

  I am called by the Worker after assigning its transaction ID to the order
  assigned for block-generation. The key here is given as a cell of form
  [n | snapshot_path] where `n` is a natural number representing the `n`-th
  transaction candidate - i.e. the order of the transaction the Worker is
  occupied with - and snapshot_path is a Nock list, i.e. an improper list
  of form `[... | 0]`.

  My main functionality is to make sure that the value of said transaction
  has been recorded suscessfully in the Qualified table in the following
  way:

  After subscribing to the appropriate Qualified table, I first check the
  fed-in Nock noun. If it starts with 0, I produce `:error`, per standard
  semantics. Given another Nock cell, we then read it as a key inside our
  Qualified table. If the value has already been written, we return it as
  an `{:ok, value}` tuple.

  Otherwise, if we read-off an empty list, i.e. if the appropriate
  transaction value has not been written, we wait for the message that the
  exact key has been written with some value in the table. Then we return
  the value written in an `{:ok, value}` tuple.
  """

  @spec blocking_read(Router.Addr.t(), qualified_key()) ::
          :error | {:ok, any()}
  def blocking_read(storage, key) do
    do_blocking_read(Router.Engine.get_state(storage), key)
  end

  @spec do_blocking_read(t(), qualified_key()) :: :error | {:ok, any()}
  defp do_blocking_read(storage = %__MODULE__{}, key) do
    log_info({:read, key})

    case key do
      [0 | _] ->
        :error

      [_ | _] ->
        :mnesia.subscribe({:table, storage.qualified, :simple})
        key = namespace_qualified_key(storage, key)
        tx = fn -> :mnesia.read(storage.qualified, key) end
        {:atomic, result} = :mnesia.transaction(tx)

        case result do
          [{_, ^key, value}] ->
            :mnesia.unsubscribe({:table, storage.qualified, :simple})
            {:ok, value}

          [] ->
            receive do
              {:mnesia_table_event, {:write, {_, ^key, value}, _}} ->
                :mnesia.unsubscribe({:table, storage.qualified, :simple})
                {:ok, value}
            end
        end

      _ ->
        :error
    end
  end

  ############################################################
  #                         Snapshots                        #
  ############################################################

  @doc """
  I provide the main functionality of rescuing latest snapshot value of a
  given key.

  Given a {storage, snapshot} pair and a key, I search for the order of the
  key as captured by the snapshot using `in_snapshot/2`. Afterwards, I
  check the value of said key in said position in the provided table,
  returning said value in the standard mnesia format.
  """

  @spec get_at_snapshot(snapshot(), order_key()) ::
          :absent | {:ok, qualified_value()}
  def get_at_snapshot(snapshot = {storage, _}, key) do
    position = in_snapshot(snapshot, key)

    checked_read_at(storage, key, position)
  end

  @doc """
  I am the function searching for a key position inside of a given Ordering
  snapshot.

  Given a {key, snapshot} tuple I go though the snapshot searching for the
  key in the format we store it inside the tables (i.e. namespaced). After
  which, I return the stored value, i.e. the latest Order at the point of
  snapshot-taking.
  """

  @spec in_snapshot(snapshot(), order_key()) :: nil | non_neg_integer()
  def in_snapshot({storage, snapshot}, key) do
    List.keyfind(snapshot, namespace_key(storage, key), 0, {nil, nil})
    |> elem(1)
  end

  ############################################################
  #                     Conceptual Helpers                   #
  ############################################################

  @spec calculate_order([{any(), any(), number()}]) :: number()
  defp calculate_order([{_, _, order}]), do: order + 1
  defp calculate_order([]), do: 1

  @spec checked_read_at(t(), Noun.t(), non_neg_integer()) ::
          :absent | {:ok, qualified_value()}
  defp checked_read_at(storage = %Storage{}, key, order) do
    log_info({:get_order, order})

    with {:atomic, [{_, [^order, ^key | 0], value}]} <-
           do_read_at_order_tx(storage, key, order) do
      case value do
        # the key has been removed
        :absent ->
          :absent

        _ ->
          {:ok, value}
      end
    else
      _ -> :absent
    end
  end

  @doc """
  I am the read_order function.

  Given a storage structure and a key, I read the order of said key
  by literally asking mnesia to read the Ordering table at the key which
  is gotten by appending the storage namespace to the initially given key.

  I return a list of three-tuples with following elements:

  1) Ordering table name
  2) Key
  3) Order number
  """

  @spec read_order(t(), order_key()) ::
          list({atom(), Noun.t(), non_neg_integer()})
  def read_order(storage = %__MODULE__{}, key) do
    lst = :mnesia.read(storage.order, namespace_key(storage, key))

    for {at, key, val} <- lst do
      {:ok, key} = denamespace_key(storage, key)
      {at, key, val}
    end
  end

  @spec write_at_order(t(), Noun.t(), qualified_value(), non_neg_integer()) ::
          :ok
  defp write_at_order(storage = %__MODULE__{}, key, value, order) do
    :mnesia.write({storage.order, namespace_key(storage, key), order})

    :mnesia.write(
      {storage.qualified,
       namespace_qualified_key(storage, qualified_key(key, order)), value}
    )
  end

  @doc """
  I am the read_keyspace_order function.

  My functionality is very similar to `read_order/2` yet instead of using
  the mnesia `read` I use `select` and pick out any keys of list type in
  the Ordering table which might contain inside the given keylist.

  I return the `{:atomic, lst}` tuple where the latter value is a list of
  lists where the head are the denamespaced keys containing our original
  query keylist and the tail being the order.
  """

  @spec read_keyspace_order(t(), list()) :: result(list(list(any())))
  def read_keyspace_order(storage = %__MODULE__{}, key_query) do
    lst = read_keyspace_at(storage.order, namespace_key(storage, key_query))

    with {:atomic, lst} <- lst do
      lst =
        for [key, value | tl] <- lst do
          {:ok, key} = denamespace_key(storage, key)
          [key, value | tl]
        end

      {:atomic, lst}
    end
  end

  @doc """
  I am the read_at_order function.

  I provide detailed reading functionality for the qualified table. Given a
  storage structure, a key, and an order, I read the Qualified mnesia table
  at a value which on a high-level corresponds to reading the key value at
  that particular order, which is, of course, represented by the order
  being part of the searched key at the mnesia level.

  I return a list of 3-tuples with elements:

  1) Name of Qualified Table
  2) Key (note here "key" has different semantics than top-level API as
          noted above, it has a lot of extra info such as the order)
  3) Value of the key
  """

  @spec read_at_order(t(), Noun.t(), non_neg_integer()) ::
          list({atom(), qualified_key(), qualified_value()})
  def read_at_order(storage = %__MODULE__{}, key, order) do
    lst =
      :mnesia.read(
        storage.qualified,
        namespace_qualified_key(storage, qualified_key(key, order))
      )

    for {at, key, val} <- lst do
      {:ok, key} = denamespace_qualified_key(storage, key)
      {at, key, val}
    end
  end

  # Add a namespace to a qualified key
  @spec namespace_qualified_key(t(), qualified_key()) :: qualified_key()
  defp namespace_qualified_key(storage = %__MODULE__{}, [hd, key | tl]) do
    [hd, namespace_key(storage, key) | tl]
  end

  # Add a namespace to a key
  @spec namespace_key(t(), Noun.t()) :: Noun.t()
  defp namespace_key(storage = %__MODULE__{}, key) do
    storage.namespace ++ key
  end

  @spec does_storage_actually_exist(atom()) :: boolean()
  defp does_storage_actually_exist(table) do
    # Just asking if the table exists doesn't ensure rocksdb tables do
    # exist, so we try to query the table
    case :mnesia.transaction(fn -> :mnesia.first(table) end) do
      {:aborted, {:no_exists, _table}} ->
        false

      _ ->
        true
    end
  end

  ############################################################
  #                          Helpers                         #
  ############################################################

  # Remove the namespace from a qualified key
  @spec denamespace_qualified_key(t(), qualified_key()) ::
          {:ok, qualified_key()} | :error
  defp denamespace_qualified_key(storage = %__MODULE__{}, [hd, key | tl]) do
    with {:ok, key} <- denamespace_key(storage, key) do
      {:ok, [hd, key | tl]}
    end
  end

  # Remove the namespace from a key
  @spec denamespace_key(t(), Noun.t()) :: {:ok, Noun.t()} | :error
  defp denamespace_key(storage = %__MODULE__{}, key) do
    improper_drop(key, storage.namespace)
  end

  # Drop the prefix indicated by the list from the given noun
  @spec improper_drop(Noun.t(), maybe_improper_list()) ::
          {:ok, maybe_improper_list()} | :error
  defp improper_drop(lst, []), do: {:ok, lst}

  defp improper_drop([hd1 | tl1], [hd2 | tl2]) when hd1 == hd2 do
    improper_drop(tl1, tl2)
  end

  defp improper_drop(_, _), do: :error

  @spec qualified_key(Noun.t(), non_neg_integer()) :: qualified_key()
  defp qualified_key(key, order), do: [order, key | 0]

  @spec read_keyspace_at(any(), list()) :: result(list(list(any())))
  defp read_keyspace_at(table, key_query) do
    keys = create_equality_query(key_query)

    query_tx = fn ->
      :mnesia.select(table, [{{:_, :"$2", :"$3"}, keys, [:"$$"]}])
    end

    log_info({:read_all, key_query})
    :mnesia.transaction(query_tx)
  end

  @spec create_equality_query(list()) :: list()
  defp create_equality_query(keys) do
    keys
    |> Enum.zip(1..length(keys)//1)
    |> Enum.map(fn {key, num} -> create_equality_constraint(key, num) end)
  end

  @spec create_equality_constraint(any(), integer()) ::
          {:==, {:hd, any()}, any()}
  defp create_equality_constraint(key, num) do
    {:==, nth_car(num), key}
  end

  @spec nth_car(integer()) :: {:hd, any()}
  defp nth_car(num) do
    {:hd, 2..num//1 |> Enum.reduce(:"$2", fn _, acc -> {:tl, acc} end)}
  end

  ############################################################
  #                      Logging Info                        #
  ############################################################

  defp log_info({:get_order, order}) do
    Logger.add(nil, :debug, "Getting at order: #{inspect(order)}")
  end

  defp log_info({:put_order, order}) do
    Logger.add(nil, :debug, "Putting at order: #{inspect(order)}")
  end

  defp log_info({:read, key}) do
    Logger.add(nil, :info, "Regular blocking read at key: #{inspect(key)}")
  end

  defp log_info({:read_all, keys}) do
    Logger.add(nil, :info, "Reading key_space at: #{inspect(keys)}")
  end

  defp log_info({:delete_key, key}) do
    Logger.add(nil, :debug, "Deleting key: #{inspect(key)}")
  end

  defp log_info({:restarting_storage, table}) do
    Logger.add(
      nil,
      :error,
      "Table: #{inspect(table)} is in an inconsistent state." <>
        "!!!ATTENTION!!! please run Anoma.Mnesia.fresh_storage() to restart storage"
    )
  end
end
