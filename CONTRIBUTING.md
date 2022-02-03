# Contributing to Anoma

Thank you for the interest in contributing to Anoma!

All contributors are expected to follow the [Code of Conduct](CODE_OF_CONDUCT.md).

Any contributions such as issue reports, pull requests  for code or documentation, or feature requests are welcome. Please note that the code should be kept up-to-date with the documentation.

## Opening a pull request

Every pull request should start with an issue. A pull request should be as atomic as reasonably possible. Please use the Github's syntax in the PR's description to link the issue (e.g. `closes #123`).

### Changelog

To track changes in Anoma and provide a nicely formatted change log with the releases, we utilize the [unclog CLI tool](https://github.com/informalsystems/unclog). Please do not modify the [change log](CHANGELOG.md) in your PRs, this file will be updated by the repository maintainers.

With every PR, please make a separate commit that adds a record in the `.changelog` directory with a section that this PR belongs to together with a high-level description of the change.

The section should either be one of the following choices:

- `CI`
- `bug-fixes`
- `docs`
- `features`
- `improvements`
- `testing`

To add a change log entry using `unclog`, you can fill in the following command (prefer to use the issue number, for which the `--pull-request` argument may be omitted):

```shell
unclog add \
  --id           <branch name (omit owner name before the /)> \
  --section      <section name> \
  # only include one of --issue-no or --pull-request
  --issue-no     <issue number> \
  --pull-request <PR number> \
  --message      <message>
```

The message should be a high-level description of the changes that should explain the scope of the change and affected components to Anoma's users (while git commit messages should target developers).

If none of the sections fit, new sections may be added. To find the existing section names, you can use e.g.:

```shell
for i in $(ls -d .changelog/*/*/); do basename "$i"; done
```

## Development priorities

If you’d like to follow the development or contribute with new or unimplemented features, we recommend to check [the pinned issues](https://github.com/anoma/anoma/issues) that are set to tracking issues in current focus of the ledger, intent gossiper and matchmaker team.
