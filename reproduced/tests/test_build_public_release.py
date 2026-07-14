#!/usr/bin/env python3
"""Unit tests for the history-free public release builder."""

from __future__ import annotations

import importlib.util
import json
import subprocess
import tempfile
import unittest
from pathlib import Path


SCRIPT = (
    Path(__file__).resolve().parents[1]
    / "scripts"
    / "00_setup"
    / "build_public_release.py"
)
SPEC = importlib.util.spec_from_file_location("build_public_release", SCRIPT)
assert SPEC and SPEC.loader
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class PublicReleaseBuilderTests(unittest.TestCase):
    def test_private_and_protected_paths_are_excluded(self) -> None:
        self.assertTrue(MODULE.is_excluded(Path(".mcp.json")))
        self.assertTrue(MODULE.is_excluded(Path("release_manifest.json")))
        self.assertTrue(MODULE.is_excluded(Path(".project-management/notes.md")))
        self.assertTrue(MODULE.is_excluded(Path("renv/.gitignore")))
        self.assertTrue(MODULE.is_excluded(Path("reproduced/docs/roadmaps/internal.md")))
        self.assertTrue(MODULE.is_excluded(Path("reproduced/scripts/debug/debug.R")))
        self.assertTrue(MODULE.is_excluded(Path("reproduced/scripts/tasking/tasks.py")))
        self.assertTrue(
            MODULE.is_excluded(
                Path("reproduced/docs/references/chapter7_saom_analysis_report.md")
            )
        )
        self.assertTrue(MODULE.is_excluded(Path("reproduced/data/raw/participants.csv")))
        self.assertFalse(MODULE.is_excluded(Path("reproduced/data/raw/README.md")))
        self.assertTrue(
            MODULE.is_excluded(Path("reproduced/docs/status/2025-09-19_phase_tracker.md"))
        )
        self.assertFalse(
            MODULE.is_excluded(
                Path("reproduced/docs/status/2026-07-14_public_release_verification.md")
            )
        )
        self.assertFalse(
            MODULE.is_excluded(
                Path("reproduced/analyses/chapter8_interventions/README.md")
            )
        )

    def test_audit_flags_secrets_and_local_paths(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            token = "sbp" + "_" + "a" * 24
            (root / "unsafe.md").write_text(
                "token: " + token + "\npath: /" + "Users/example/private\n",
                encoding="utf-8",
            )
            issues = MODULE.audit_tree(root)
            self.assertTrue(any("Supabase token" in issue for issue in issues))
            self.assertTrue(any("local absolute path" in issue for issue in issues))

    def test_audit_allows_documented_placeholders(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "example.md").write_text(
                'api_key = "placeholder-value"\n', encoding="utf-8"
            )
            self.assertEqual(MODULE.audit_tree(root), [])

    def test_audit_flags_quoted_json_secret_assignments(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            key_name = "SEMANTIC" + "_SCHOLAR_API_KEY"
            secret_value = "live" + "-credential-value-1234567890"
            (root / "config.json").write_text(
                '{"' + key_name + '": "' + secret_value + '"}\n',
                encoding="utf-8",
            )
            issues = MODULE.audit_tree(root)
            self.assertTrue(any("possible assigned secret" in issue for issue in issues))

    def test_audit_flags_unquoted_secret_assignments(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            key_name = "BRAVE" + "_SEARCH_API_KEY"
            secret_value = "live" + "-credential-value-1234567890"
            (root / "config.yml").write_text(
                key_name + ": " + secret_value + "\n",
                encoding="utf-8",
            )
            issues = MODULE.audit_tree(root)
            self.assertTrue(any("possible assigned secret" in issue for issue in issues))

    def test_audit_checks_relative_markdown_links_and_images(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "docs/assets").mkdir(parents=True)
            (root / "docs/assets/figure.svg").write_text("<svg/>\n", encoding="utf-8")
            (root / "docs/README.md").write_text(
                "[good](assets/figure.svg)\n"
                "![also good](assets/figure.svg)\n"
                "[external](https://example.com)\n"
                "[outside](../../private.md)\n"
                "[missing](missing.md)\n",
                encoding="utf-8",
            )
            issues = MODULE.audit_markdown_links(root)
            self.assertEqual(
                issues,
                [
                    "Markdown link leaves release tree: docs/README.md -> ../../private.md",
                    "broken Markdown link: docs/README.md -> missing.md",
                ],
            )

    def test_reference_sanitizer_removes_private_roadmap_panel(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            for filename in ("reference.html", "index.html"):
                with self.subTest(filename=filename):
                    path = Path(tmp) / filename
                    path.write_text(
                        "<main>\n"
                        "    <!-- ==================== ROADMAP TAB ==================== -->\n"
                        "private planning\n"
                        "  </main>\n",
                        encoding="utf-8",
                    )
                    MODULE.sanitize_release_file(path, Path("docs") / filename)
                    self.assertEqual(path.read_text(encoding="utf-8"), "<main>\n\n  </main>\n")

    def test_build_creates_history_free_filtered_manifest(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            repo = base / "source"
            output = base / "release"
            (repo / "reproduced/data/raw").mkdir(parents=True)
            (repo / ".project-management").mkdir()
            (repo / "README.md").write_text("safe\n", encoding="utf-8")
            (repo / "reproduced/data/raw/README.md").write_text(
                "Protected data are not distributed.\n", encoding="utf-8"
            )
            (repo / "reproduced/data/raw/participants.csv").write_text(
                "participant_id\nprivate\n", encoding="utf-8"
            )
            (repo / ".project-management/notes.md").write_text(
                "private notes\n", encoding="utf-8"
            )
            (repo / ".mcp.json").write_text("{}\n", encoding="utf-8")
            subprocess.run(["git", "init", "-q"], cwd=repo, check=True)
            subprocess.run(["git", "add", "."], cwd=repo, check=True)
            chapter8 = repo / "reproduced/analyses/chapter8_interventions/README.md"
            chapter8.parent.mkdir(parents=True)
            chapter8.write_text("Experimental and outside the verified workflow.\n", encoding="utf-8")
            (repo / "release_manifest.json").write_text("{}\n", encoding="utf-8")

            MODULE.build(repo, output, force=False)

            self.assertTrue((output / "README.md").is_file())
            self.assertTrue((output / "reproduced/data/raw/README.md").is_file())
            self.assertFalse((output / "reproduced/data/raw/participants.csv").exists())
            self.assertFalse((output / ".project-management").exists())
            self.assertFalse((output / ".mcp.json").exists())
            self.assertFalse((output / "release_manifest.json").is_symlink())
            self.assertTrue(
                (output / "reproduced/analyses/chapter8_interventions/README.md").is_file()
            )
            self.assertFalse((output / ".git").exists())
            manifest = json.loads((output / "release_manifest.json").read_text(encoding="utf-8"))
            self.assertEqual(manifest["file_count"], 3)
            self.assertNotIn(
                "release_manifest.json",
                {entry["path"] for entry in manifest["files"]},
            )


if __name__ == "__main__":
    unittest.main()
