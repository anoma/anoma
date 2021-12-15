# Drone script

This folder contains some helper scripts to manage the pipeline.

## update-drone-config.py

This script is useful when you have to modify either `.drone.yml` or one of the `Makefile`. It should be modified only when adding a new `Makefile` or script to call within the CI. This is also possible by commenting `update ci` on the PR.

### How to run:
- Ask for aws credential and [setup aws-cli with a profile](https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-quickstart.html#cli-configure-quickstart-profiles).
- [Install poetry](https://python-poetry.org/docs/).
- [Install drone cli](https://docs.drone.io/cli/install/).
- Run `poetry install`.
- Run `poetry run python update-drone-config.py`. Check options with `--help` flag. 
    - If you need to use a profile different than `default` use `--aws-profile`.
- Check that `.drone.yml` has changed.
- Commit and push.


