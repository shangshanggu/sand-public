#!/usr/bin/env python3
"""Validate that packages declared in renv.lock are downloadable."""
from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Iterable, List, Tuple
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen


def url_available(url: str) -> bool:
    """Return True if the URL responds with a successful status code."""
    request = Request(url, method="HEAD")
    try:
        with urlopen(request) as response:  # nosec B310 - HEAD request only
            return 200 <= response.status < 400
    except HTTPError as err:
        if err.code == 405:  # Method not allowed, retry with ranged GET
            fallback = Request(url, method="GET", headers={"Range": "bytes=0-0"})
            try:
                with urlopen(fallback) as response:  # nosec B310 - range-limited GET
                    return 200 <= response.status < 400
            except HTTPError:
                return False
            except URLError:
                return False
        return False
    except URLError:
        return False


def find_unavailable(packages: Iterable[Tuple[str, str, str]], repo_url: str) -> List[Tuple[str, str, str, str]]:
    """Check package tarballs against the main and archive repositories."""
    missing: List[Tuple[str, str, str, str]] = []
    for package, version, label in packages:
        base = f"{repo_url}/src/contrib/{package}_{version}.tar.gz"
        if url_available(base):
            continue
        archive = f"{repo_url}/src/contrib/Archive/{package}/{package}_{version}.tar.gz"
        if url_available(archive):
            continue
        missing.append((package, version, base, archive))
    return missing


def parse_lockfile(path: Path) -> Tuple[str, List[Tuple[str, str, str]]]:
    data = json.loads(path.read_text())
    repo_url = data["R"]["Repositories"][0]["URL"].rstrip("/")
    packages: List[Tuple[str, str, str]] = []
    for name, record in sorted(data["Packages"].items()):
        if record.get("Source") != "Repository":
            continue
        packages.append((record["Package"], record["Version"], name))
    return repo_url, packages


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "lockfile",
        nargs="?",
        default=Path("reproduced/renv.lock"),
        type=Path,
        help="Path to the renv lockfile",
    )
    args = parser.parse_args()

    lockfile: Path = args.lockfile
    if not lockfile.exists():
        raise SystemExit(f"Lockfile not found: {lockfile}")

    repo_url, packages = parse_lockfile(lockfile)
    missing = find_unavailable(packages, repo_url)

    if missing:
        print("The following packages are unavailable from the configured repository:")
        for package, version, base, archive in missing:
            print(f"  - {package} {version}\n    {base}\n    {archive}")
        return 1

    print(
        f"Verified {len(packages)} packages from {repo_url} are available via main or archive tarballs.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
