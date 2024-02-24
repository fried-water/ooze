#! /usr/bin/env python3

import subprocess
import sys
import os
import argparse
import re

def run_command_on_file(command, file_path):
    try:
        result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True, text=True)
        return (True, result.stdout, result.stderr)
    except subprocess.CalledProcessError as e:
        return (False, e.stdout, e.stderr)

def run_directory(directory, test_regex):
    cmd = ["build/regtest/regtest", "run"]
    files = [os.path.join(directory, f) for f in os.listdir(directory) if os.path.isfile(os.path.join(directory, f))]

    pass_count = 0
    fail_count = 0

    for file in files:
        if not test_regex.search(file): continue
        success, stdout, stderr = run_command_on_file(cmd + [file], file)
        if success:
            pass_count += 1
        else:
            print(f"test failed: {file}\n")
            if(len(stdout) > 0): print(stdout, end="")
            if(len(stderr) > 0): print(stderr, end="")
            print("")
            fail_count += 1

    print(f"Passed: {pass_count}, Failed: {fail_count}")

def main():
    parser = argparse.ArgumentParser(description='Run regtests')

    parser.add_argument('-r', '--regex', type=str, default='.*',
                        help='Optional regex pattern (default: .*)')

    parser.add_argument('-d', '--dir', type=str, default='regtest/pass',
                        help='Test directory to run (default: regtest/pass)')

    args = parser.parse_args()

    try:
        run_directory(args.dir, re.compile(args.regex))
    except re.error:
        parser.error(f"Invalid regex pattern: {args.regex}")


if __name__ == "__main__":
    main()
