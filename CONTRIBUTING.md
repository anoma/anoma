# Anoma

Thank you for your interest in working on Anoma!

This document serves as an easy way to get used to the codebase.

This document assumes some level of familrity with Elixir and it's
environemnt, but not too much.

## Running multiple IEX's in the same Image/Environemnt

It is sometimes useful to have multiple terminals/IEX's in the same
running system, or perhaps to connect to a running deploy anoma
Instance. We can connect to other IEX instances in this way:

```bash
MIX_ENV=test iex --sname b@localhost --cookie anoma -S mix
# open a new terminal
MIX_ENV=test iex --remsh b@localhost --sname c@localhost --cookie anoma -S mix
```

This also allows you to connect from [livebook](https://livebook.dev)
by using the above cookie `anoma` under the `runtime` config of
livebook.

## Figuring out what a module does

Α good start is by calling `h` on the module from within one's IEX
instance.

```elixir
iex(mari@Gensokyo)63> h Anoma.Node

                                   Anoma.Node

I am the supervisor for Anoma Nodes

## Required Arguments
  • name - name to register the nodes
...
```

However, this typically doesn't show off how one uses said
module. Thankfully, the codebase is setup in such a way that one can
always interactively paly with any given module.

This is done by simply checking out the tests folder, and finding the
module you wish to learn to learn about.


For example, let us learn about the mempool. In the codebase currently
this can be found as `test/node/mempool_test.exs`, note that even if
this gets out of date, you should be able to do this with any file!

The first thing one can do to run things interactively is by taking
all the imports of the file and running it locally

In this case I input the following from the file into IEX.

I also make sure to include an extra `import ExUnit.Assertions` so
that assertions can be copied and pasted to IEX

```elixir
# output redacted for length
iex(mari@Gensokyo)64>   alias Anoma.Storage
iex(mari@Gensokyo)65>   alias Anoma.Node.Storage.Communicator, as: Scom
iex(mari@Gensokyo)66>   alias Anoma.Node.Executor.Communicator, as: Ccom
iex(mari@Gensokyo)67>   alias Anoma.Node.Mempool.Communicator, as: Mcom
iex(mari@Gensokyo)68>   import TestHelper.Nock
iex(mari@Gensokyo)69> import ExUnit.Assertions
```

After the imports are done, we copy the `setup_all` if this section
exists

```elixir
# output redacted due to brevity
iex(mari@Gensokyo)70>     storage = %Anoma.Storage{
...(mari@Gensokyo)70>       qualified: AnomaTest.Mempool.Qualified,
...(mari@Gensokyo)70>       order: AnomaTest.Mempool.Order
...(mari@Gensokyo)70>     }
iex(mari@Gensokyo)71>
iex(mari@Gensokyo)72>     name = :mempool
iex(mari@Gensokyo)73>     snapshot_path = [:my_special_nock_snaphsot | 0]
iex(mari@Gensokyo)74>
iex(mari@Gensokyo)75>     {_, node} =
...(mari@Gensokyo)75>       if Process.whereis(:mempool_mempool_com) do
...(mari@Gensokyo)75>         {:error, Anoma.Node.com_names(name)}
...(mari@Gensokyo)75>       else
...(mari@Gensokyo)75>         Anoma.Node.start_link(
...(mari@Gensokyo)75>           name: name,
...(mari@Gensokyo)75>           snapshot_path: snapshot_path,
...(mari@Gensokyo)75>           storage: storage,
...(mari@Gensokyo)75>           block_storage: :mempool_blocks
...(mari@Gensokyo)75>         )
...(mari@Gensokyo)75>       end
iex(mari@Gensokyo)76>
iex(mari@Gensokyo)77>     [node: node]
```

From here we can run any tests in the file by copying those as well!

What is even better is that we can copy parts of tests to setup an
area to play with the code to figure out what is going well with our
other tools.

This is a great way for learning any API in the codebase as you can
get hands on what each function and message does.

```elixir
iex(mari@Gensokyo)78>     Ccom.subscribe(node.executor, self())
:ok
iex(mari@Gensokyo)79>     Mcom.hard_reset(node.mempool)
:ok
iex(mari@Gensokyo)80>     pid_one = Mcom.tx(node.mempool, increment).pid
#PID<0.371.0>
iex(mari@Gensokyo)81>     pid_two = Mcom.tx(node.mempool, increment).pid
#PID<0.372.0>
iex(mari@Gensokyo)82> Mcom.execute(node.mempool)
{:ok, 2}
iex(mari@Gensokyo)83> flush
{:"$gen_cast",
 {:process_done, {#Reference<0.0.34179.904691850.4087414785.25606>, :error}}}
{:"$gen_cast", {:process_done, #PID<0.372.0>}}
{:"$gen_cast",
 {:process_done, {#Reference<0.0.34179.904691850.4087414787.25008>, :error}}}
{:"$gen_cast", {:process_done, #PID<0.371.0>}}
:ok
iex(mari@Gensokyo)84> Mcom.state(node.mempool)
{%Anoma.Node.Mempool.Communicator{primary: :mempool_mempool},
 %Anoma.Node.Mempool.Primary{
 ...
 }}
```

Further since the data is live, we can use tools like `:observer` to
view the processes, and see general state dumping commands.

For databases I've found that `Anoma.Mnesia` is a good tool along with
`:observer` for seeing what is currently in database table.


## Test Conventions

Since the figuring out section mentions convient ways of firuging out
how any module works, it is important to write tests so this can
always be done.

The principle is quite simple:

1. if you have a `setup_all`, and export names to a test, make sure
   the test names the exports the same

   ```elixir
   setup_all do
     ...
     node = ...
     [node: node]
   end

   test "successful process", %{node: node} do
      ...
   end
   ```
    - If this convention is not followed, then the user can not simply
      copy and paste lines to figure out how modules are used.

2. Try to write the `setup_all` to not crash upon running it again
   ```elixir
   setup_all do
    ...
    {_, node} =
      if Process.whereis(:mempool_mempool_com) do
        {:error, Anoma.Node.com_names(name)}
      else
        Anoma.Node.start_link(
          name: name,
          snapshot_path: snapshot_path,
          storage: storage,
          block_storage: :mempool_blocks
        )
      end
     ...
   end
   ```
   - Here we check if the process is running. This way if it is
     already in IEX we simply don't distrurb it but rename it to point
     to the correct one we wish to operate over.
   - If we did not do this check the other commands may fail and IEX
     may not be trapped to continue.
   - `mix test` will not catch this
3. Try to make tests somewhat idempotent
   ```elixir
   test ... %{node：node} do
     Mcom.hard_reset(node.mempool)
   end
   ```
   - Here we begin our test by reseting the storage. This way, if we
     run the test multiple times, it doesn't suddenly fail
   - running `mix test` won't catch this if the test is isolated from
     each other, so please do try to keep things independently
     idempotent
4. Try to name things
   - This lets the IEX session play with data, so it's good to
     overally bind if you find the data interesting

## Test Coverage

One can see the test coverage of any module by:

`mix test --cover`

Ideally any new code coming in should try to strive for at least 90%
coverage, but if that can not be done, do make tests showing off how
the given module ought to be used.

## Quick Testing
Using the command line or through one's editor is quite slow, I
recommend not using it, instead here are some alternatives

If one is fine running all tests, then the following works:

```bash
mix test
```

or

```elixir
% MIX_ENV=test iex -S mix

iex(1)> Mix.Tasks.Test.run([]) # runs all tests
iex(2)> r AnomaTest.PartialTx  # reload the tests we care about
iex(3)> Mix.Tasks.Test.run([]) # runs only the single module
# ExUnit.run can also be ran equivalently as well after the first run!
```

Where after each tests it caches, so you will need to recompile the
test you want to rerun. Overall this works fine.

Further one can run specific tests or filtered categories this way as well:

```elixir

% MIX_ENV=test iex -S mix

iex(1)> # Running a single test
iex(2)> Mix.Tasks.Test.run([])
iex(3)> AnomaTest.Node."test node works"(nil)
iex(4)> ExUnit.configure(exclude: [:test], include: [describe: "hello"]) # test all hello describe blocks
iex(5)> r AnomaTest.PartialTx
iex(6)> ExUnit.run # or Mix.Tasks.Test.run([])

```

However if running every test at startup is slow, then the following
will be a better way to load the tests.

```elixir
% iex -S mix

iex(1)> ExUnit.start
iex(2)> c "test/partialtx_test.exs" # only once
iex(3)> ExUnit.configure(exclude: [:test], include: [line: 12]) # test line
iex(4)> ExUnit.run
iex(5)> r AnomaTest.PartialTx
iex(6)> ExUnit.run
```

If one is wanting to run specific tests, see the `Test Conventions`
section for how best to run and play with tests.

## Codebase Organization

For any long run Erlang servers, they should go below the `Anoma.Node`
module.

This makes it easy to see what distributed actors there are on the
system, and keeps the non agent code obvious.

## Codebase Conventions

This section covers various conventions found throughout the codebase.

### Communicators

Communicators are a small actor that acts as a secrtary for any given
componenet. They handle work of a secretary, notifying clients of new
development and forwarding data to who they are the secretary of.

As of this writing, they are a bit verbose in that it requires copying
most public methods for whom they are secretarying for. A macro might
be able to automate this better in the future.

### Instrumentation

Sadly Erlang/Elixir lack an object system and anything akin to CLOS's
[around-methods](https://www.algo.be/cl/documents/clos-guide.html#meth-comb)
thus a pattern has arose, if a flag to many actors is sent in for
`instrumentation`, then text during computation will be printed to the
user's screen.

As of this writing `instrumentation` is a boolean that turns on all
debugging flags, but in the present/future it may have evolved into a
decent filtering system to see where computation is happening in the
multi agent system easily.

It is advised to put `instrumentaiton` flags into your actors so they
may be debugged in this manner.
