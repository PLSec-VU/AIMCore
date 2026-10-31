#!/usr/bin/env python3
"""Rerun the checker on the benchmark binaries and print its verdicts.

Each program proof under proof-smt/Proof/SMT/Stepper passes only if the
checker returns the verdict its name states, so a green build is the result.
This script deletes those modules' compiled files so that GHC rebuilds them,
runs the build with Bitwuzla, and prints one row per checker run with its
time. GHC compares source hashes, so touching the sources would not do.

Usage: benchmark/eval.py [--bitwuzla PATH] [--csv FILE]
"""

import argparse
import csv
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PROOF_SOURCES = ROOT / "proof-smt"
CHECKER_PROOFS = {"Relational", "RelMem", "Invariant"}
PROGRAM_PROOFS = sorted(
    path.relative_to(PROOF_SOURCES).with_suffix("")
    for path in (PROOF_SOURCES / "Proof/SMT/Stepper").rglob("*.hs")
    if path.stem not in CHECKER_PROOFS
)
COMPILED_SUFFIXES = [".hi", ".o", ".dyn_hi", ".dyn_o"]

VERDICTS = [
    ("NoLeak", "no leak"),
    ("Leaks", "leak"),
    ("OutOfFuel", "out of fuel"),
    ("OneStepShort", "out of fuel"),
]

COMPILING = re.compile(r"Compiling (Proof\.SMT\.Stepper\.\S+)")
EVALUATING = re.compile(r"SymFC evaluating '([^']+)'")
ELAPSED = re.compile(r"\*\*\* SymFC: Elapsed time: ([0-9.]+)")


def verdict_of(proof):
    for marker, verdict in VERDICTS:
        if marker in proof:
            return verdict
    return None


def run_build(bitwuzla):
    env = dict(os.environ, SBV_Z3=bitwuzla, SBV_Z3_OPTIONS="--produce-models")
    for build_dir in ROOT.glob(".stack-work/dist/*/*/build"):
        for module in PROGRAM_PROOFS:
            for suffix in COMPILED_SUFFIXES:
                build_dir.joinpath(module).with_suffix(suffix).unlink(missing_ok=True)
    build = subprocess.Popen(
        ["stack", "build", "--test", "--no-run-tests", "--force-dirty"],
        cwd=ROOT,
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        errors="replace",
    )
    rows = []
    module = proof = None
    for line in build.stdout:
        if found := COMPILING.search(line):
            module = found.group(1).removeprefix("Proof.SMT.Stepper.")
            if module in CHECKER_PROOFS:
                module = None
        elif found := EVALUATING.search(line):
            proof = found.group(1)
        elif (found := ELAPSED.search(line)) and proof:
            verdict = verdict_of(proof)
            if verdict and module:
                rows.append((module, proof, verdict, float(found.group(1))))
            proof = None
    return build.wait(), rows


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--bitwuzla", default=shutil.which("bitwuzla"))
    parser.add_argument("--csv", type=Path)
    args = parser.parse_args()
    if not args.bitwuzla:
        sys.exit("bitwuzla not found; pass --bitwuzla PATH")

    status, rows = run_build(args.bitwuzla)
    rows.sort()
    width = max((len(proof) for _, proof, _, _ in rows), default=10)
    for module, proof, verdict, seconds in rows:
        print(f"{module:14} {proof:{width}} {verdict:12} {seconds:8.1f} s")
    if args.csv:
        with args.csv.open("w", newline="") as out:
            writer = csv.writer(out)
            writer.writerow(["program", "check", "verdict", "seconds"])
            writer.writerows(rows)
    if status != 0:
        sys.exit("the build failed, so at least one verdict is not the one its proof states")


if __name__ == "__main__":
    main()
