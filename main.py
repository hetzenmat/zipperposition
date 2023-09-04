#!/usr/bin/env python3

from concurrent import futures
import sys
from pathlib import Path
import subprocess as sp
import logging
from typing import List
import re
import json

# --mode ho-optimistic --timeout 30 -i tptp -o tptp --tptp-def-as-rewrite --rewrite-before-cnf=true --boolean-reasoning=bool-hoist --bool-select=LI
# --dont-simplify --disable-checks

TIMEOUT = None


def is_theorem(output: List[str]) -> bool:
    return any(map(lambda s: s.startswith('% SZS status Theorem')
                             or s.startswith('% SZS status Unsatisfiable')
                             or s.startswith('% SZS status ContradictoryAxioms'), output))


def is_timeout(output: List[str]) -> bool:
    return any(map(lambda s: s.startswith('% SZS status ResourceOut'), output))

def gave_up(output: List[str]) -> bool:
    return any(map(lambda s: s.startswith('% SZS status GaveUp'), output))


def has_constraints(output: List[str]) -> bool:
    return any(map(lambda s: s.startswith('Empty clause with constraints'), output))


def run_single(logger: logging.Logger, idx: int, zip_path: Path, problem_path: Path, out_dir: Path, args: str):
    logger.info(f'Starting problem {idx + 1}: {problem_path}')

    result_path = out_dir / Path("/".join(problem_path.parts[problem_path.parts.index('problems') + 1:]))

    result_path.mkdir(parents=True, exist_ok=True)
    args = args.strip("'")
    completed = sp.run(f"timeout {TIMEOUT + 30}s {zip_path} {problem_path} {args}", capture_output=True, shell=True)

    result = completed.stdout.decode(encoding='ascii', errors='ignore')

    (result_path / "stdout").write_text(result)
    (result_path / "stderr").write_text(completed.stderr.decode(encoding='ascii', errors='ignore'))

    lines = list(result.splitlines())

    if completed.returncode == 124:
        r = "TO killed"
    elif is_timeout(lines):
        r = "TO"
    elif is_theorem(lines):
        r = "THM"
        #if has_constraints(lines):
        #    r += " C"
    elif gave_up(lines):
        r = "GAVE UP"
    else:
        r = "??"

    logger.info(f'Finished problem {idx + 1}: {problem_path} with result {r}')

    return str(problem_path), r


def main():
    logging.basicConfig(level=logging.INFO)
    logger = logging.getLogger(__name__)

    zip_path = Path(sys.argv[1]).expanduser()
    if not zip_path.is_file():
        print("zipperposition executable not found")
        sys.exit()

    problem_dir = Path(sys.argv[2]).expanduser()
    if not problem_dir.exists():
        print("Problem dir not found")
        sys.exit()

    out_dir = Path(sys.argv[3]).expanduser()
    if out_dir.exists():
        print("out dir already exists!")
        sys.exit()

    args = sys.argv[4]

    global TIMEOUT
    TIMEOUT = int(re.search(r"--timeout\s+(\d+)", args).group(1))

    problem_paths = problem_dir.rglob('*')
    problem_paths = list(filter(lambda p: p.is_file() and p.suffix != '.ax', problem_paths))

    with futures.ProcessPoolExecutor() as executor:
        tasks = [executor.submit(run_single, logger, idx, zip_path, path, out_dir, args) for idx, path in
                 enumerate(problem_paths)]

    stats = dict()
    for task in tasks:
        path, result = task.result()
        stats[path] = result

    (out_dir / 'overall.txt').write_text(json.dumps(stats, sort_keys=True, indent=4))

if __name__ == "__main__":
    main()
