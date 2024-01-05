# Anoma

This is an implementation of the Anoma protocol, whose specs can be
found [here](https://specs.anoma.net/alpha).

## Following Development

Work is merged into `base` on a bi-weekly (once every two weeks)
schedule.

Development can be followed in multiple ways:

1. [Issues are put into the project overview](https://github.com/orgs/anoma/projects/19)
   - This is a good way to see what work is assigned and the various
     views into how goals are being met
2. [Promise Graph from the project overview](https://specs.anoma.net/projects/anoma-19.html)
   - This is the same information as `1.` but using our own promise
     graph tooling. This is kept up to date hourly.
3. [What's Cooking on Anoma](https://github.com/orgs/anoma/projects/20 "A good view on how topics are progressing throughout a cycle")
4. [Issues](https://github.com/anoma/anoma/issues) and [pull requests](https://github.com/anoma/anoma/pulls)
   - This is good for viewing new issues and work coming in, but the
     other views are typically a better way to view this

## Installation

To install the dependencies as well as Anoma run:

```bash
mix deps.get
mix compile
```

To start an Anoma instance run one of these:

```bash
iex -S mix # starts an interactive shell
mix run --no-halt # starts a non-interactive shell
```

See the Contributing section for how to get the best use of the
interactive shell.

## Contributing

Please read the [CONTRIBUTING.md](./CONTRIBUTING.md) for in depth
details about the codebase.

### Git

This codebase follows a git style similar to
[git](https://git-scm.com/) or
[linux](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git).

New code should be based on `base`, and no attempt to keep it up to
sync with `main` should be had. When one's topic is ready, just submit
a PR on github and a maintiner will handle any merge conflicts.

There are bi-weekly releases, so do not be afraid if a maintainer says
the PR is merged but it's still oepn, this just means that it's merged
into `next` or `main` and will be inclueded in the next scheduled
release.

For more information on a smooth git experience check out the [git section in CONTRIBUTING.md](./CONTRIBUTING.md##git)

Happy hacking, and don't be afraid to submit patches.
