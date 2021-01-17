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
    _check_with_user("Check that test has passed.")
    _release(version)


def _get_version_to_release():
    version_pattern = re.compile(r"^version: (\S+)$", re.MULTILINE)
    package = pathlib.Path("package.yaml").read_text()
    return version_pattern.search(package).group(1)


def _check_with_user(message):
    input(f"{message} (control+C to abort)")


def _check_that_current():
    subprocess.run(["git", "fetch"], check=True)
    subprocess.run(["git", "diff", "--exit-code", "HEAD", "origin/main"], check=True)


def _release(version):
    subprocess.run(
        ["git", "tag", "--annotate", version, "--message", version], check=True
    )
    subprocess.run(["stack", "upload", "."], check=True)
    subprocess.run(["git", "push", "origin", version], check=True)


if __name__ == "__main__":
    main()
