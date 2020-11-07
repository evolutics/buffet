#!/usr/bin/env python3

import os
import pathlib
import re
import subprocess


def main():
    os.chdir(pathlib.Path(os.path.realpath(__file__)).parent.parent)

    version = _get_version_to_release()
    _check_with_user(f"Version to be released is '{version}'.")
    _check_that_current()
    _check_with_user("Check that build has passed.")
    _tag(version)
    _check_with_user("Wait until tag build has passed.")
    _upload()


def _get_version_to_release():
    version_pattern = re.compile(r"^version: (\S+)$", re.MULTILINE)
    with pathlib.Path("package.yaml").open() as package:
        return version_pattern.search(package.read()).group(1)


def _check_with_user(message):
    input(f"{message} (control+C to abort)")


def _check_that_current():
    subprocess.run(["git", "fetch"], check=True)
    subprocess.run(["git", "diff", "--exit-code", "HEAD", "origin/master"], check=True)


def _tag(version):
    subprocess.run(
        ["git", "tag", "--annotate", version, "--message", version], check=True
    )
    subprocess.run(["git", "push", "origin", version], check=True)


def _upload():
    subprocess.run(["stack", "upload", "."], check=True)


if __name__ == "__main__":
    main()
