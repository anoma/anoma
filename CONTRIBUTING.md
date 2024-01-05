# Anoma

Thank you for your interest in working on Anoma!

This document serves as an easy way to get used to the codebase.

This document assumes some level of familiarity with Elixir and it's
environment, but not too much.

## Running multiple IEX's in the same Image/Environments

It is sometimes useful to have multiple terminals/IEX's in the same
running system, or perhaps to connect to a running deploy Anoma
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
always interactively play with any given module.

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

Since the figuring out section mentions convenient ways of figuring out
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
     already in IEX we simply don't disturb it but rename it to point
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
   - Here we begin our test by resenting the storage. This way, if we
     run the test multiple times, it doesn't suddenly fail
   - running `mix test` won't catch this if the test is isolated from
     each other, so please do try to keep things independently
     idempotent
4. Try to name things
   - This lets the IEX session play with data, so it's good to
     overall bind if you find the data interesting

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

Communicators are a small actor that acts as a Secretary for any given
component. They handle work of a secretary, notifying clients of new
development and forwarding data to who they are the secretary of.

As of this writing, they are a bit verbose in that it requires copying
most public methods for whom they are secretary's for. A macro might
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

It is advised to put `instrumentation` flags into your actors so they
may be debugged in this manner.

## git

Git is a decent version control(VC) system, however there are ways to
make the VC process a lot smoother.

### Terminology

- `topic` - this is any branch that serves to fix some problem in the
  codebase

- `feature` - some new concept to the codebase. Many topics can serve
  to fulfill one feature.

- `release` - A release of the code. This is a git `tag` on `main`
  that signifies a new version of the software. This typically bumps
  base to the latest release as well

- `base` - the base branch one should base work off of

- `main` - sometimes called `master`, is the branch that prepares for
  a release.

- `next` - a branch that has a superset of features that will be
  included in the next release

- `maint` - a maintnance branch that will be updated if bugs are found

- `integreation branch` - a topic that merges a bunch of other topics
### General Principles
These are some general principles which should help maintainers easily
integrate your code, and have your work help out other devs on the
codebase.

#### Do not include unrelated changes into your commits
- For example, if you see some unrelated bug in the same file as your
  own, don't fix it in your general commit, make a new topic based on
  when the bug was introduced and merge that into your topic if it
  impacts your topic.

- This makes reviewing much easier as the reviewer can read your
  commit message and see changes only related to that included

#### Make topics early and often!
This allows your work to be incrementally integrated into a
release. If you put all your work into one topic, bug fixes and all,
then the following will occur

1. The changes will not be reviewed properly
   - On big projects with tight deadlines, sometimes some feature
     *X* is wanted. However if *X* is a single topic with a messy
     history, the only options are either to scrap the feature or
     accept it as is poor code in all.
   - If this was properly split up parts of *X* could be merged
     now, with the more controversial features being held up in
     *next*, without having to sacrifice the quality of the
     codebase.

2. Other team members can not share similar work
   - Often a lot of different tasks, may find the same
     deficiencies in the codebase.
   - For a real example, let's take the following commit and topic

      ```example
     802ab9e * anoma/mariari/nock-testing-file Move the helper functions
     73bfd7d * v0.3.0
      2 files changed, 44 insertions(+), 34 deletions(-)
     lib/test_helper/nock.ex | 43 +++++++++++++++++++++++++++++++++++++++++++
     test/nock_test.exs      | 35 +----------------------------------
     ```

     Here we find that we `mariari` moved some testing functions from
     `test/` to `lib/`, as elixir tests don't share code in the best
     way. This allows other files in `test/` to reuse the same
     functions that were previously found in `test/nock_test.exs`.  In
     fact there are multiple topics that ended up using this. Both the
     executor topic and worker topic

       - `8de0c7e anoma/mariari/worker`
       - `1d6bc99 anoma/mariari/executor`

      both needed this. Since the worker relies upon the executor,
     they both don't merge in this topic separately, but if they
     were separate they would want to share this change.

  - If these changes were orchestrated by different people, then
    they would have made this change twice! Meaning that in the git
    history the work has been done in different commits! This means
    that when it comes time to merge in work, there will be a
    conflict between these two. Rather than being able to reuse
    other's work and save other devs time, this will come up when
    reviewers read the code, with unrelated changes, or when the
    maintainers try to merge things together and find annoying
    conflicts

3. Work can not be incrementally included
4. Others might just do the work before you
  - If one is too slow on finishing their topic and making it one big
    commit, then someone else might redo the same work and put it up
    for review, but instead of them reusing your code, they wrote it
    from scratch, wasting both your time and their time.

#### Base topics on base

Basing code on `main` has the following errors:

1. Code merged in main before a release may turn out to have issues
2. Git merges and conflict resolutions lead to spurious base points
3. Other topics can not reuse your code
4. Useless temporal history is had

Basing on someone's topic that you require and will merge in anyways
is fine.

##### Code merged in main before a release may turn out to have issues

Imagine main has the following history

```
52b44a6 *   main Merge branch feature-Y into next
        |\
4381bd3 | * Topic-Y
        |/
73bfd7d *  v0.3.0 base
```

and your code hapens to be base on 52b44a6

```
7dabf44 *   my-cool-feature
52b44a6 *   main Merge branch feature-Y into next
        |\
4381bd3 | * Topic-Y
        |/
73bfd7d *  v0.3.0 base
```

Later before a release, we find out that *Topic-Y* has issues, and any
code that is based on *Topic-Y* will have to sit this release
out. Normally to check for this, the protocol is quite simple we just:

1. Do not include any topics that are based on *Topic-Y* or merges
   *Topic-Y* into the release
2. Pull any topic based on *Topic-Y* from `main`

becomes muddied, as if one's topic was based on `main` after *Topic-Y*
is in, then it's unclear if that topic is unaffected.

Thus *my-cool-feature* may be cut from the release, even if it was
perfectly fine and did not rely on *Topic-Y*.

##### Git merges and conflict resolutions lead to spurious base points

Further, if we have two topics *my-feature-x* and *my-feature-y* based
on `main`, then the history would look something like this

```
0438922 *   main Merge branch 'topic-y'
        |\
546a8f9 | * topic-y Added a feature that conflcits with X!
90d91e7 * |   Merge branch 'topic-x'
        |\ \
        | |/
        |/|
bc4b2a1 | * topic-x Added a cool feature
2dd991a * |   Merge branch 'proper-topic'
        |\ \
        | |/
        |/|
8087564 | * proper-topic Add a new feature
13b3e4a * |   Merge branch 'ray/mnesia-attach'
        |\ \
        | |/
        |/|
97b6ef7 | * ray/mnesia-attach mnesia:
        |/
73bfd7d * v0.3.0 base

```

When *topic-x* and *topic-y* have a conflict, the shared base of their base is

```bash
4 taichi@Gensokyo:~/Documents/Work/Repo/anoma-all git:main: % git merge-base topic-x topic-y
13b3e4a215ea6222a1b1092ad242d3fa31e7040b
```

which is `13b3e4a * | Merge branch 'ray/mnesia-attach'` and not
`73bfd7d * v0.3.0 anoma/base`, meaning that when a conflict is shown
in the merge `0438922`, then the diff from a 3 way diff will show the
mnesia changes, potentially making it unclear to others way the
potentially issues may be.

##### Other topics can not reuse your code

It is a bad idea to base code on `main`, as `main` contains random
merged topics before a release. This makes it so other topics who wish
to use yours also has to merge all the random topics on `main`.

This is easy to see with the following example:

```
f098de0 * simple-config-change I update the config to be more general for others
7d9a059 *   anoma/main Merge branch 'major-changes'
        |\
5b16844 | * major-changes I commit serious structural changes to the codebase
        |/
73bfd7d * v0.3.0 anoma/base

```

Here we have a topic *major-changes* that makes all sorts of changes,
and since we based our code off main, these are all included in
simple-config-change.

However imagine we wish to overhaul the configuration a bit

```
c4b563f * configuration-upgrade I create some basic configuration changes
73bfd7d * v0.3.0 anoma/base
```

Now if we wish to merge in *simple-config-change* we have


```
f6230df *   configuration-upgrade Merge branch 'simple-config-change'
        |\
f098de0 | * simple-config-change I update the config to be more general for others
7d9a059 | *   main Merge branch 'major-changes'
        | |\
5b16844 | | * major-changes I commit serious structural changes to the codebase
        | |/
c4b563f * / I create some basic configuration changes
        |/
73bfd7d * v0.3.0 anoma/base
```

Besides having a spurious main merged into our topic now, we are
forced to deal with *major-changes* causing various conflicts with
your topic, making this merge untenable.

Meaning that this code has to be recreated in *configuration-upgrade*
instead of reusing *simple-config-change*, fixing the problem in 2
places, and having a conflict when it comes time for a release.

##### Useless temporal history is had

As we can see in the previous examples, when we base off of `main`, we
end up in a scenario, where the date in which someone is branching is
baked into the code. As maintainers we don't care about when the code
was made, just the fact that it was. Thus this is a bit of history
that simply adds noise

#### Merge other people's topics into yours

If you need some work that is already merged into `next` or `main`,
simply merge that topic into yours! Since the bases are well situated,
you will only deal with reasonable conflicts that you should have
context for.

#### Base bug fixes on the commit that introduced the bug

Basing a bug fix on when the bug is introduced is superior than basing
it on the latest release, as this means that it can be merged into any
`maint` branches we may have.

For example:

```
73bfd7d * v0.3.0 Anoma 0.3.0
...
10f8636 * v0.2.0 Anoma 0.2.0
...
34fcd78 * v0.1.0 Release v0.1.0
```


if a bug was found in a topic between `v0.1.0` and `v0.2.0`, and we
based it on when the bug was found we can merge it on `v0.2.0` and
have `v0.2.1` release from there. And have a `v0.3.1` release as well.

```
2373834 *   v0.2.1 Merge branch 'bug-fix' into HEAD
        |\
da24431 | * bug-fix fix bug
10f8636 * | v0.2.0 Anoma 0.2.0
```

```
19c6f03 *   v0.3.1 Merge branch 'bug-fix' into HEAD
        |\
da24431 | * bug-fix fix bug
73bfd7d * | v0.3.0 Anoma 0.3.0
```

notice how we can merge this in with no conflicts!

### Naming conventions
Name your topic like `name/feature` to avoid clashing with other
people's topics.

There are some standard branches that do not follow this pattern but
those are described in the `Terminology` section of this document
