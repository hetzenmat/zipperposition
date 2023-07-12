from concurrent import futures
import sys
from pathlib import Path
import subprocess as sp


def is_theorem(output: str) -> bool:
    return any(map(lambda s: s.startswith('# SZS status Theorem'), output.split('\n')))


def is_timeout(output: str) -> bool:
    return any(map(lambda s: s.startswith('# SZS status ResourceOut'), output.split('\n')))


def run_single(zip_path : Path, problem_path: Path, out_dir: Path):
    result_path = out_dir / Path("/".join(problem_path.parts[problem_path.parts.index('problems') + 1:]))

    result_path.parents[0].mkdir(parents=True, exist_ok=True)

    completed = sp.run([ str(zip_path)
                       , str(problem_path)
                       , '--mode ho-optimistic'
                       , '--timeout 30'
                       , '-i tptp'
                       , '-o tptp'
                       , '--tptp-def-as-rewrite'
                       , '--rewrite-before-cnf=true'
                       , '--boolean-reasoning=bool-hoist'
                       , '--bool-select=LI'], capture_output=True)

    result = completed.stdout.decode(encoding='ascii', errors='ignore')

    result_path.write_text(result)

    if is_theorem(result):
        r = "THM"
    elif is_timeout(result):
        r = "TO"
    else:
        r = "??"

    return str(problem_path), r


def main():
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

    problem_paths = problem_dir.rglob('*')
    problem_paths = list(filter(lambda p: p.is_file() and p.suffix != '.ax', problem_paths))

    with futures.ProcessPoolExecutor() as executor:
        tasks = [executor.submit(run_single, zip_path, path, out_dir) for path in problem_paths]

    (out_dir / 'overall.txt').write_text('\n'.join(sorted([str(task.result()) for task in tasks])))


if __name__ == "__main__":
    main()
