#!/usr/bin/env python3
"""Build and audit a clean SAND public-release candidate from the worktree."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import shutil
import subprocess
from pathlib import Path
from urllib.parse import unquote


EXCLUDED_PREFIXES = (
    ".git/",
    ".project-management/",
    ".kiro/",
    ".vscode/",
    ".cursor/",
    "renv/",
    "shang_thesis_may/",
    "reproduced/docs/archive/",
    "reproduced/docs/roadmaps/",
    "reproduced/scripts/debug/",
    "reproduced/scripts/tasking/",
)
PUBLIC_STATUS_FILES = {
    "reproduced/docs/status/2026-07-14_public_release_verification.md",
}
EXCLUDED_FILES = {
    ".mcp.json",
    "AGENTS.md",
    "CLAUDE.md",
    "release_manifest.json",
    "reproduced/data/proxy/outcomes.csv",
    "reproduced/data/proxy/participants.csv",
    "generate_r_inventory.py",
    "reproduced/run_ch4_to_ch8.sh",
    "reproduced/run_ch4_to_ch8_clean.sh",
    "reproduced/scripts/00_setup/create_sample_chapter4_raw_data.R",
    "reproduced/scripts/00_setup/create_sample_chapter7_baseline.R",
    "reproduced/scripts/00_setup/stage_intervention_inputs.py",
    "reproduced/scripts/extract_saom_coefficients.R",
    "reproduced/scripts/run_saom_real.R",
    "reproduced/docs/references/chapter4_sample_data.md",
    "reproduced/docs/references/chapter4_sample_data_structure.md",
    "reproduced/docs/references/chapter7_code_review.md",
    "reproduced/docs/references/chapter7_saom_analysis_report.md",
    "reproduced/docs/references/chapter7_saom_restart.md",
    "reproduced/docs/references/chapter8_intervention_configuration.md",
    "reproduced/docs/references/codebase_improvements_2025-12-02.md",
    "reproduced/docs/references/data_dictionary_skeleton.md",
    "reproduced/docs/references/legacy_chapter5_nam_schema.md",
    "reproduced/docs/references/proxy_data_requirements.md",
    "reproduced/docs/references/task_command_framework.md",
    "reproduced/docs/references/thesis_packaging_workflow.md",
}
TEXT_SUFFIXES = {
    "", ".cff", ".css", ".csv", ".gitignore", ".html", ".json", ".md",
    ".py", ".r", ".rprofile", ".sh", ".svg", ".tex", ".txt", ".yaml", ".yml",
}
SECRET_PATTERNS = {
    "private key": re.compile(r"-----BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY-----"),
    "GitHub token": re.compile(r"(?:ghp|gho|ghu|ghs|ghr|github_pat)_[A-Za-z0-9_]{20,}"),
    "OpenAI-style key": re.compile(r"\bsk-(?:proj-)?[A-Za-z0-9_-]{20,}"),
    "Slack token": re.compile(r"\bxox[baprs]-[A-Za-z0-9-]{20,}"),
    "AWS access key": re.compile(r"\bAKIA[0-9A-Z]{16}\b"),
    "Supabase token": re.compile(r"\bsbp_[A-Za-z0-9_-]{20,}"),
    "JWT": re.compile(r"\beyJ[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}\b"),
}
GENERIC_SECRET = re.compile(
    r"(?i)[\"']?[A-Za-z0-9_.-]*(?:api[_-]?key|access[_-]?token|auth[_-]?token|password|client[_-]?secret)"
    r"[A-Za-z0-9_.-]*[\"']?\s*[:=]\s*(?:[\"']([^\"'\r\n]{8,})[\"']|([^\s#,}\]]{12,}))"
)
SAFE_SECRET_MARKERS = ("example", "placeholder", "changeme", "redacted", "${", "{{")
MARKDOWN_LINK = re.compile(r"!?\[[^\]]*\]\(([^)]+)\)")
URI_SCHEME = re.compile(r"^[A-Za-z][A-Za-z0-9+.-]*:")


def git_files(repo: Path) -> list[Path]:
    result = subprocess.run(
        ["git", "ls-files", "--cached", "--others", "--exclude-standard", "-z"],
        cwd=repo,
        check=True,
        capture_output=True,
    )
    return [Path(item.decode()) for item in result.stdout.split(b"\0") if item]


def is_excluded(rel: Path) -> bool:
    posix = rel.as_posix()
    if posix.startswith("reproduced/docs/status/"):
        return posix not in PUBLIC_STATUS_FILES
    if posix in EXCLUDED_FILES or any(posix.startswith(prefix) for prefix in EXCLUDED_PREFIXES):
        return True
    if posix.startswith("reproduced/data/raw/") and posix != "reproduced/data/raw/README.md":
        return True
    return False


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def audit_markdown_links(root: Path) -> list[str]:
    issues: list[str] = []
    for path in sorted(root.rglob("*.md")):
        text = path.read_text(encoding="utf-8", errors="replace")
        for match in MARKDOWN_LINK.finditer(text):
            raw_target = match.group(1).strip()
            if raw_target.startswith("<") and ">" in raw_target:
                target = raw_target[1 : raw_target.index(">")]
            else:
                target = raw_target.split(maxsplit=1)[0]
            if not target or target.startswith("#") or URI_SCHEME.match(target):
                continue
            local_target = unquote(target.split("#", 1)[0].split("?", 1)[0])
            if not local_target:
                continue
            resolved_target = (path.parent / local_target).resolve()
            release_root = root.resolve()
            if resolved_target != release_root and release_root not in resolved_target.parents:
                issues.append(
                    f"Markdown link leaves release tree: {path.relative_to(root).as_posix()} -> {target}"
                )
            elif not resolved_target.exists():
                issues.append(
                    f"broken Markdown link: {path.relative_to(root).as_posix()} -> {target}"
                )
    return issues


def audit_tree(root: Path) -> list[str]:
    issues: list[str] = []
    files = sorted(path for path in root.rglob("*") if path.is_file() or path.is_symlink())
    for path in files:
        rel = path.relative_to(root)
        posix = rel.as_posix()
        lower_name = path.name.lower()
        if path.is_symlink():
            target = path.resolve()
            if root.resolve() not in target.parents:
                issues.append(f"external symlink: {posix}")
            continue
        if path.stat().st_size > 20 * 1024 * 1024:
            issues.append(f"file exceeds 20 MiB: {posix}")
        if lower_name in {".env", ".mcp.json", "id_rsa", "id_ed25519"}:
            issues.append(f"forbidden filename: {posix}")
        if posix.startswith("reproduced/data/raw/") and lower_name != "readme.md":
            issues.append(f"protected-data path: {posix}")
        if path.suffix.lower() not in TEXT_SUFFIXES and path.name not in {
            "Dockerfile", "Makefile", ".Rprofile", ".gitattributes", ".gitignore"
        }:
            continue
        try:
            text = path.read_text(encoding="utf-8")
        except UnicodeDecodeError:
            continue
        if re.search(r"/" + r"Users/[^/\s]+/", text):
            issues.append(f"local absolute path: {posix}")
        for label, pattern in SECRET_PATTERNS.items():
            if pattern.search(text):
                issues.append(f"possible {label}: {posix}")
        for match in GENERIC_SECRET.finditer(text):
            value = (match.group(1) or match.group(2)).lower()
            if not any(marker in value for marker in SAFE_SECRET_MARKERS):
                issues.append(f"possible assigned secret: {posix}")
                break
    issues.extend(audit_markdown_links(root))
    return sorted(set(issues))


def sanitize_release_file(path: Path, rel: Path) -> None:
    """Remove private-only sections from files that also serve public readers."""
    if rel.as_posix() != "docs/reference.html":
        return
    text = path.read_text(encoding="utf-8")
    start_marker = "    <!-- ==================== ROADMAP TAB ==================== -->"
    end_marker = "\n  </main>"
    start = text.find(start_marker)
    if start == -1:
        return
    end = text.find(end_marker, start)
    if end == -1:
        raise SystemExit("Could not locate the private roadmap section in docs/reference.html")
    path.write_text(text[:start] + text[end:], encoding="utf-8")


def build(repo: Path, output: Path, force: bool) -> None:
    if output.exists():
        if not force:
            raise SystemExit(f"Output already exists: {output}. Use --force to replace it.")
        shutil.rmtree(output)
    output.mkdir(parents=True)

    copied: list[Path] = []
    for rel in git_files(repo):
        source = repo / rel
        if is_excluded(rel) or not source.exists() or source.is_dir():
            continue
        destination = output / rel
        destination.parent.mkdir(parents=True, exist_ok=True)
        if source.is_symlink():
            destination.symlink_to(source.readlink())
        else:
            shutil.copy2(source, destination)
        sanitize_release_file(destination, rel)
        copied.append(rel)

    issues = audit_tree(output)
    if issues:
        shutil.rmtree(output)
        raise SystemExit("Release audit failed:\n- " + "\n- ".join(issues))

    manifest = {
        "release_state": "unversioned-candidate",
        "file_count": len(copied),
        "files": [
            {"path": rel.as_posix(), "sha256": sha256(output / rel)}
            for rel in sorted(copied)
        ],
    }
    (output / "release_manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
    )
    print(f"Built audited release candidate with {len(copied)} files at {output}")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--force", action="store_true")
    args = parser.parse_args()
    repo = Path(__file__).resolve().parents[3]
    build(repo, args.output.expanduser().resolve(), args.force)


if __name__ == "__main__":
    main()
