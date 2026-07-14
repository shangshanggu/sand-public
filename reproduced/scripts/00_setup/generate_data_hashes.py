#!/usr/bin/env python3
"""Generate checksum manifests for CSV and RData dependencies.

This helper collects data dependencies from ``reproduced/config/thesis.yml``
(raw inputs, processed artefacts, and chapter
requirements), hashing any ``.csv`` or ``.RData`` files that currently exist,
and writing the results to ``reproduced/logs/data_hashes.json``.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import sys
from collections import defaultdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Mapping, MutableMapping, Sequence, Set, Tuple

REPO_ROOT = Path(__file__).resolve().parents[3]
REPRO_ROOT = REPO_ROOT / "reproduced"

for path in (REPO_ROOT, REPRO_ROOT):
    if str(path) not in sys.path:
        sys.path.insert(0, str(path))

from scripts.utils.yaml_loader import load_yaml_minimal

DEFAULT_CONFIG_PATH = REPRO_ROOT / "config" / "thesis.yml"
DEFAULT_OUTPUT_PATH = REPRO_ROOT / "logs" / "data_hashes.json"
DEFAULT_EXTENSIONS = {".csv", ".rdata"}


def _normalise_path(path: Path) -> Path:
    return path if path.is_absolute() else (Path.cwd() / path).resolve()


def _relative_to_repo(path: Path) -> str:
    try:
        return str(path.relative_to(REPO_ROOT))
    except ValueError:
        return str(path)


def _parse_extensions(raw: str | None) -> Set[str]:
    if not raw:
        return set(DEFAULT_EXTENSIONS)
    tokens = {token.strip() for token in raw.split(",") if token.strip()}
    parsed = set()
    for token in tokens:
        parsed.add(token.lower() if token.startswith(".") else f".{token.lower()}")
    return parsed or set(DEFAULT_EXTENSIONS)


def _get_data_mode(config: Mapping[str, object]) -> str:
    env_mode = os.getenv("SAND_DATA_MODE", "").strip().lower()
    if env_mode:
        return env_mode
    data_section = config.get("data", {})
    if isinstance(data_section, Mapping):
        mode = data_section.get("mode")
        if isinstance(mode, str) and mode.strip():
            return mode.strip().lower()
    return "real"


def _collect_config_sources(config: Mapping[str, object]) -> Tuple[List[Tuple[str, Path]], List[Tuple[str, Path]]]:
    directories: List[Tuple[str, Path]] = []
    files: List[Tuple[str, Path]] = []

    data_mode = _get_data_mode(config)
    data_section = config.get("data", {})
    proxy_dir = None
    if isinstance(data_section, Mapping):
        proxy_dir = data_section.get("proxy_dir")

    project = config.get("project")
    raw_dir_value = None
    if isinstance(project, Mapping):
        paths_cfg = project.get("paths")
        if isinstance(paths_cfg, Mapping):
            raw_dir_value = paths_cfg.get("raw_data_dir")
            raw_dir = paths_cfg.get("raw_data_dir")
            raw_override = proxy_dir if data_mode == "proxy" and isinstance(proxy_dir, str) else None
            for key in (
                "raw_data_dir",
                "processed_data_dir",
                "intermediate_data_dir",
                "external_data_dir",
            ):
                value = paths_cfg.get(key)
                if key == "raw_data_dir" and raw_override:
                    directories.append((f"data.proxy_dir", (REPO_ROOT / raw_override).resolve()))
                    continue
                if isinstance(value, str):
                    directories.append((f"project.paths.{key}", (REPO_ROOT / value).resolve()))

    if isinstance(data_section, Mapping):
        raw_files = data_section.get("raw_files")
        if isinstance(raw_files, Mapping):
            for name, value in raw_files.items():
                if isinstance(value, str):
                    if (
                        data_mode == "proxy"
                        and isinstance(proxy_dir, str)
                        and isinstance(raw_dir_value, str)
                        and value.startswith(raw_dir_value)
                    ):
                        value = proxy_dir + value[len(raw_dir_value) :]
                    files.append((f"data.raw_files.{name}", (REPO_ROOT / value).resolve()))

    chapters = config.get("chapters")
    if isinstance(chapters, Mapping):
        for chapter_key, chapter_data in chapters.items():
            if not isinstance(chapter_data, Mapping):
                continue
            required_inputs = chapter_data.get("required_inputs")
            if isinstance(required_inputs, Sequence):
                for index, item in enumerate(required_inputs):
                    if isinstance(item, str):
                        if (
                            data_mode == "proxy"
                            and isinstance(proxy_dir, str)
                            and isinstance(raw_dir_value, str)
                            and item.startswith(raw_dir_value)
                        ):
                            item = proxy_dir + item[len(raw_dir_value) :]
                        files.append(
                            (
                                f"chapters.{chapter_key}.required_inputs[{index}]",
                                (REPO_ROOT / item).resolve(),
                            )
                        )

    return directories, files


def _add_cli_includes(
    include_paths: Sequence[Path] | None,
    directories: List[Tuple[str, Path]],
    files: List[Tuple[str, Path]],
) -> None:
    if not include_paths:
        return
    for raw_path in include_paths:
        path = _normalise_path(raw_path)
        label = "cli.include"
        if path.is_dir():
            directories.append((label, path))
        else:
            files.append((label, path))


def _discover_files(
    directories: Sequence[Tuple[str, Path]],
    files: Sequence[Tuple[str, Path]],
    extensions: Set[str],
) -> Tuple[MutableMapping[Path, Set[str]], List[Dict[str, object]], List[Dict[str, object]]]:
    file_sources: MutableMapping[Path, Set[str]] = defaultdict(set)
    missing_map: MutableMapping[str, Dict[str, object]] = {}
    skipped_map: MutableMapping[str, Dict[str, object]] = {}

    def record_missing(path: Path, kind: str, label: str, note: str | None = None) -> None:
        rel_path = _relative_to_repo(path)
        entry = missing_map.setdefault(
            rel_path,
            {
                "path": rel_path,
                "kind": kind,
                "sources": set(),
            },
        )
        entry["sources"].add(label)
        if note and not entry.get("note"):
            entry["note"] = note

    def record_skipped(path: Path, label: str, reason: str, extension: str | None = None) -> None:
        rel_path = _relative_to_repo(path)
        entry = skipped_map.setdefault(
            rel_path,
            {
                "path": rel_path,
                "reason": reason,
                "sources": set(),
            },
        )
        entry["sources"].add(label)
        if extension and not entry.get("extension"):
            entry["extension"] = extension

    for label, directory in directories:
        if not directory.exists():
            record_missing(directory, "directory", label)
            continue
        if not directory.is_dir():
            record_missing(
                directory,
                "directory",
                label,
                note="configured path is not a directory",
            )
            continue
        for candidate in directory.rglob("*"):
            if candidate.is_file() and candidate.suffix.lower() in extensions:
                file_sources[candidate.resolve()].add(label)

    for label, file_path in files:
        if not file_path.exists():
            record_missing(file_path, "file", label)
            continue
        if not file_path.is_file():
            record_missing(
                file_path,
                "file",
                label,
                note="configured path is not a file",
            )
            continue
        if file_path.suffix.lower() not in extensions:
            record_skipped(
                file_path,
                label,
                reason="unsupported-extension",
                extension=file_path.suffix.lower(),
            )
            continue
        file_sources[file_path.resolve()].add(label)

    missing = []
    for rel_path in sorted(missing_map):
        entry = missing_map[rel_path]
        entry["sources"] = sorted(entry["sources"])
        missing.append(entry)

    skipped = []
    for rel_path in sorted(skipped_map):
        entry = skipped_map[rel_path]
        entry["sources"] = sorted(entry["sources"])
        skipped.append(entry)

    return file_sources, missing, skipped


def _hash_file(path: Path) -> str:
    sha256 = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            sha256.update(chunk)
    return sha256.hexdigest()


def _build_manifest(
    file_sources: Mapping[Path, Set[str]],
    missing: Sequence[Mapping[str, object]],
    skipped: Sequence[Mapping[str, object]],
    config_path: Path,
    extensions: Set[str],
) -> Dict[str, object]:
    files_payload: List[Dict[str, object]] = []
    for file_path in sorted(file_sources):
        stat = file_path.stat()
        files_payload.append(
            {
                "path": _relative_to_repo(file_path),
                "sha256": _hash_file(file_path),
                "size_bytes": stat.st_size,
                "modified": datetime.fromtimestamp(stat.st_mtime, tz=timezone.utc).isoformat(),
                "sources": sorted(file_sources[file_path]),
            }
        )

    manifest: Dict[str, object] = {
        "generated_at": datetime.now(tz=timezone.utc).isoformat(),
        "config": _relative_to_repo(config_path),
        "extensions": sorted(extensions),
        "files": files_payload,
    }
    if missing:
        manifest["missing"] = list(missing)
    if skipped:
        manifest["skipped"] = list(skipped)
    return manifest


def parse_arguments(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate SHA-256 hashes for configured CSV and RData files."
    )
    parser.add_argument(
        "--config",
        type=Path,
        default=DEFAULT_CONFIG_PATH,
        help="Path to the thesis configuration file (default: reproduced/config/thesis.yml)",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=DEFAULT_OUTPUT_PATH,
        help="Destination for the checksum manifest (default: logs/data_hashes.json)",
    )
    parser.add_argument(
        "--include",
        type=Path,
        action="append",
        default=None,
        help="Additional files or directories to hash (can be specified multiple times)",
    )
    parser.add_argument(
        "--extensions",
        type=str,
        default=",".join(sorted(DEFAULT_EXTENSIONS)),
        help="Comma-separated list of file extensions to hash (default: .csv,.rdata)",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_arguments(argv)

    config_path = _normalise_path(args.config)
    if not config_path.exists():
        print(f"Configuration file not found: {config_path}", file=sys.stderr)
        return 2

    extensions = _parse_extensions(args.extensions)

    try:
        config = load_yaml_minimal(config_path)
    except Exception as exc:  # pragma: no cover - defensive path
        print(f"Failed to load configuration: {exc}", file=sys.stderr)
        return 2

    directories, files = _collect_config_sources(config)
    _add_cli_includes(args.include, directories, files)

    file_sources, missing, skipped = _discover_files(directories, files, extensions)

    output_path = _normalise_path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    manifest = _build_manifest(file_sources, missing, skipped, config_path, extensions)
    output_path.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")

    print(
        f"Wrote {len(manifest['files'])} hashes to {output_path}"
        + (f"; {len(missing)} missing" if missing else "")
        + (f"; {len(skipped)} skipped" if skipped else "")
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
