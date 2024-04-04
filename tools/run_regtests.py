#! /usr/bin/env python3

import subprocess
import os
import argparse
import re

def run_command_on_file(command):
    try:
        result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True, text=True)
        return (True, result.stdout, result.stderr)
    except subprocess.CalledProcessError as e:
        return (False, e.stdout, e.stderr)

def run_directory(executable, directory, test_regex):
    cmd = [executable, "run"]
    files = [os.path.join(directory, f) for f in os.listdir(directory) if os.path.isfile(os.path.join(directory, f))]

    pass_count = 0
    fail_count = 0

    for file in files:
        if not test_regex.search(file): continue
        success, stdout, stderr = run_command_on_file(cmd + [file])
        if(len(stdout) > 0): print(stdout)
        if(len(stderr) > 0): print(stderr)
        if success:
            pass_count += 1
        else:
            print(f"test failed: {file}\n")
            fail_count += 1

    print(f"Passed: {pass_count}, Failed: {fail_count}")

def main():
    parser = argparse.ArgumentParser(description='Run regtests')

    parser.add_argument('-r', '--regex', type=str, default='.*',
                        help='Optional regex pattern (default: .*)')

    parser.add_argument('-d', '--dir', type=str, default='regtest/pass',
                        help='Test directory to run (default: regtest/pass)')

    parser.add_argument('-e', '--exec', type=str, default='build/release/regtest/regtest',
                        help='Regtest executable (default: build/release/regtest/regtest)')

    args = parser.parse_args()

    try:
        run_directory(args.exec, args.dir, re.compile(args.regex))
    except re.error:
        parser.error(f"Invalid regex pattern: {args.regex}")

if __name__ == "__main__":
    main()
