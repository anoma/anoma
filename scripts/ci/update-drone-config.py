import os
from pathlib import Path
from typing import Dict


from botocore.config import Config
import argparse

import boto3


DRONE_FILE: str = ".drone"
DRONE_FILE_SUFFIX: str = ".yml"
REPOSITORY: str = "anoma/anoma"

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


if __name__ == "__main__":
    parser = argparse.ArgumentParser("Update drone configuration.")
    parser.add_argument('--aws-profile', help='The name of the AWS profile to use.', type=str, default="default")
    args = parser.parse_args()

    boto3.setup_default_session(profile_name=args.aws_profile)

    sign_drone_config()