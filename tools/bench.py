#! /usr/bin/env python3

import subprocess
import argparse
import time
import statistics

def run_command(command):
    start = time.perf_counter();
    try:
        result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True, text=True)
        return (True, time.perf_counter() - start, result.stdout, result.stderr)
    except subprocess.CalledProcessError as e:
        return (False, time.perf_counter() - start, e.stdout, e.stderr)

def bench_script(file, count, args):
    cmd = ["build/regtest/regtest", "run", file]
    if len(args) > 0:
        cmd = cmd + ["--args"] + args

    times = []
    for _ in range(count):
        _, execution_time, _, _ = run_command(cmd)
        times.append(execution_time)

    mean = statistics.mean(times)
    median = statistics.median(times)
    stddev = statistics.stdev(times) if count > 1 else -1;

    print(f"Mean:   {mean:.4f}s")
    print(f"StdDev: {stddev:.4f}s")
    print(f"Worst:  {max(times):.4f}s")

def main():
    parser = argparse.ArgumentParser(description='Benchmark')

    parser.add_argument('-c', '--count', type=int, default='10',
                        help='How many times to run (default: 10)')

    parser.add_argument("--args", nargs='+', default=[], help="An optional list of arguments.")
    parser.add_argument('file', type=str, default='regtest/pass',
                        help='script to benchmark')

    args = parser.parse_args()
    bench_script(args.file, args.count, args.args)

if __name__ == "__main__":
    main()
