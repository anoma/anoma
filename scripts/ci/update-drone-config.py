import os
from hashlib import sha256
from pathlib import Path
from typing import Dict


from botocore.config import Config
from ruamel.yaml import YAML
import argparse

import boto3


DRONE_FILE: str = ".drone"
DRONE_FILE_SUFFIX: str = ".yml"
REPOSITORY: str = "anoma/anoma"

STEP_NAME = 'check-scripts-integrity'

files_to_check = [
    'Makefile',
    'wasm/wasm_source/Makefile',
    'wasm/vp_template/Makefile',
    'wasm/tx_template/Makefile',
    'docs/Makefile',
    'scripts/ci/pre-run.sh',
    'scripts/ci/release.sh',
    'scripts/ci/build-and-publish-docs.sh',
    'scripts/ci/audit.py',
    'scripts/ci/udeps.py',
    'wasm/checksums.py'
]

scripts_to_run = [
    'scripts/ci/pre-run.sh'
]

check_command_template = 'echo "{}  {}" | sha256sum -c -'
run_command_template = 'sh {}'

boto_config = Config(
    region_name='eu-west-1',
    retries={
        'max_attempts': 5,
        'mode': 'standard'
    }
)

client = boto3.client('ssm', config=boto_config)


def get_project_root() -> Path:
    return Path(__file__).parent.parent.parent


def get_drone_conf_path() -> Path:
    project_root = get_project_root()
    return Path(project_root, DRONE_FILE).with_suffix(DRONE_FILE_SUFFIX)


def read_drone_config(path: Path) -> Dict:
    yaml = YAML(typ='safe')
    return yaml.load_all(path)


def join_path(path: Path, filename: str) -> Path:
    return Path(path, filename)


def compute_hashes() -> Dict:
    hashes = {}

    project_root = get_project_root()
    for file_path in files_to_check:
        path = join_path(project_root, file_path)
        file_content = path.open(mode="r").read().encode("utf-8")
        hashes[file_path] = sha256(file_content).hexdigest()

    return hashes


def get_drone_token() -> str:
    try:
        return client.get_parameter(
            Name='drone_machine_secret',
            WithDecryption=True
        )['Parameter']['Value']
    except Exception as e:
        print(e)
        exit(1)


def sign_drone_config():
    project_root = get_project_root()
    token = get_drone_token()
    try:
        os.system(' '.join(["cd {} &&".format(project_root), "DRONE_TOKEN={}".format(token), "DRONE_SERVER={}".format('https://ci.heliax.dev'), "drone", "sign", "--save", REPOSITORY]))
    except Exception as e:
        print(e)
        exit(1)


def main():
    drone_config_path = get_drone_conf_path()
    drone_config = read_drone_config(drone_config_path)
    hashes = compute_hashes()

    new_configs = []

    for config in drone_config:
        if 'steps' in config:
            config_steps = config['steps'][1]
            if config_steps and config_steps['name'] == STEP_NAME:
                commands = []
                for file in files_to_check:
                    new_command = check_command_template.format(hashes[file], file)
                    commands.append(new_command)
                for file in scripts_to_run:
                    new_command = run_command_template.format(file)
                    commands.append(new_command)
                config_steps['commands'] = commands
            new_configs.append(config)

    yaml = YAML(typ='safe')
    yaml.dump_all(new_configs, drone_config_path)

    sign_drone_config()


if __name__ == "__main__":
    parser = argparse.ArgumentParser("Update drone configuration.")
    parser.add_argument('--aws-profile', help='The name of the AWS profile to use.', type=str, default="default")
    args = parser.parse_args()

    boto3.setup_default_session(profile_name=args.aws_profile)

    main()