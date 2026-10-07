"""
Check the environment satisfies flopy4's declared requirements. flopy4 is
installed with `--no-deps`, so pixi.toml lists them; this catches drift.
"""

import sys
from importlib.metadata import PackageNotFoundError, requires, version

from packaging.requirements import Requirement


def main() -> int:
    problems = []
    for spec in requires("flopy4") or []:
        req = Requirement(spec)
        if req.marker and not req.marker.evaluate({"extra": ""}):
            continue
        try:
            installed = version(req.name)
        except PackageNotFoundError:
            problems.append(f"{req.name} is not installed (flopy4 requires {req})")
            continue
        # a direct URL requirement (e.g. modflow-devtools from git) is pinned
        # in pixi.toml, not by a version range
        if not req.url and not req.specifier.contains(installed, prereleases=True):
            problems.append(f"{req.name} {installed} does not satisfy {req}")
    for problem in problems:
        print(problem)
    if not problems:
        print(f"flopy4 {version('flopy4')} requirements satisfied")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
