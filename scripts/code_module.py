#!/usr/bin/env python3
import argparse
import re
import subprocess
import sys

OBJDUMP = "/opt/homebrew/opt/llvm/bin/llvm-objdump"
INSTRUCTION_LINE = re.compile(r"^\s*([0-9a-f]+):\s+([0-9a-f]{8})\s")


def symbol_ranges(binary):
    table = subprocess.run([OBJDUMP, "-t", binary], capture_output=True, text=True, check=True).stdout
    ranges = {}
    for line in table.splitlines():
        fields = line.split()
        if ".text" in fields:
            size = fields[fields.index(".text") + 1]
            start = int(fields[0], 16)
            ranges[fields[-1]] = (start, start + int(size, 16))
    return ranges


def instructions(binary, start, stop):
    listing = subprocess.run(
        [OBJDUMP, "-d", f"--start-address={start:#x}", f"--stop-address={stop:#x}", binary],
        capture_output=True,
        text=True,
        check=True,
    ).stdout
    for line in listing.splitlines():
        match = INSTRUCTION_LINE.match(line)
        if match:
            yield int(match.group(1), 16), int(match.group(2), 16)


def code_binding(name, words):
    lines = [f"{name} :: Code", f"{name} ="]
    for addr, word in words:
        lines.append(f"  withInstruction {addr:#x} {word:#010x} $")
    lines.append("    noCode")
    return "\n".join(lines)


def entry_binding(name, addr):
    return f"{name} :: Address\n{name} = {addr:#x}"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("binary")
    parser.add_argument("module")
    parser.add_argument("functions", nargs="+", help="name=entry_symbol[,extra_symbol...]")
    args = parser.parse_args()

    ranges = symbol_ranges(args.binary)
    bindings = []
    exports = []
    for spec in args.functions:
        name, symbols = spec.split("=")
        symbols = symbols.split(",")
        words = [w for s in symbols for w in instructions(args.binary, *ranges[s])]
        exports += [f"{name}Entry", f"{name}Code"]
        bindings.append(entry_binding(f"{name}Entry", ranges[symbols[0]][0]))
        bindings.append(code_binding(f"{name}Code", words))

    header = [
        f"module {args.module}",
        "  ( " + "\n  , ".join(exports),
        "  ) where",
        "",
        "import Stepper.RelState (Code, noCode, withInstruction)",
        "import Types (Address)",
    ]
    sys.stdout.write("\n".join(header) + "\n\n" + "\n\n".join(bindings) + "\n")


main()
