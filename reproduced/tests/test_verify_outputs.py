#!/usr/bin/env python3
"""Unit tests for mode-specific output verification."""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "00_setup" / "verify_outputs.py"
SPEC = importlib.util.spec_from_file_location("verify_outputs", SCRIPT)
assert SPEC and SPEC.loader
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


def write(path: Path, content: str = "x") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


def write_json(path: Path, payload: object) -> None:
    write(path, json.dumps(payload))


class VerifyOutputsTests(unittest.TestCase):
    def proxy_fixture(self, root: Path) -> None:
        write(root / "data/proxy/list_by_wave.RData")
        write(root / "data/proxy/.realistic_proxy_data", "")
        write(root / "outputs/chapter4/data/prepared_data_sets.RData")
        write(root / "outputs/chapter4/data/network_arrays.rds")
        write_json(
            root / "outputs/chapter4/manifests/prepared_data_manifest.json",
            {
                "entries": [],
                "network_arrays": {
                    "dyadic_nonzero_counts": {"flatmates": 50, "blockmates": 200}
                },
            },
        )
        write_json(
            root / "outputs/chapter4/logs/chapter4_qa_assertions.json",
            {"synthetic_marker_present": True, "overall_pass": True},
        )
        write(
            root / "outputs/chapter5/tables/nam_summary.csv",
            "time_period,term,estimate\nTime 1,global_misperception,0.1\n",
        )
        write_json(root / "outputs/chapter5/logs/nam_validation.json", {"results": [{"passed": False}]})
        write(root / "outputs/chapter6/tables/injunctive_nam_coefficients.csv", "term,estimate\na,0.1\n")
        write_json(
            root / "outputs/chapter6/manifests/injunctive_nam_validation.json",
            {"status": "failed", "synthetic_data": True},
        )
        write_json(root / "outputs/chapter7/manifests/saom_data_manifest.json", {"inputs": []})
        write(root / "outputs/chapter7/cache/base_fit.RData")
        write_json(
            root / "outputs/chapter7/logs/saom_run_base.json",
            {
                "status": "diagnostic_failure",
                "inputs": {"placeholder": False},
                "fit_path": "reproduced/outputs/chapter7/cache/base_fit.RData",
                "tconv_max": 1.1,
                "convergence_tolerance": 0.1,
                "diagnostic_failures": ["not converged"],
                "dyadic_nonzero_counts": {"flatmates": 50, "blockmates": 200},
            },
        )

    def test_proxy_benchmark_and_convergence_differences_are_warnings(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / "reproduced"
            self.proxy_fixture(root)
            report = MODULE.Verifier(root, "proxy", (4, 5, 6, 7)).run()
            self.assertTrue(report["passed"])
            self.assertEqual(report["summary"]["failures"], 0)
            self.assertGreaterEqual(report["summary"]["warnings"], 3)
            self.assertIsNone(report["scientific_validation_passed"])

    def test_proxy_mode_rejects_degenerate_residence_covariates(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / "reproduced"
            self.proxy_fixture(root)
            run_path = root / "outputs/chapter7/logs/saom_run_base.json"
            run = json.loads(run_path.read_text(encoding="utf-8"))
            run["dyadic_nonzero_counts"] = {"flatmates": 0, "blockmates": 0}
            write_json(run_path, run)
            report = MODULE.Verifier(root, "proxy", (7,)).run()
            self.assertFalse(report["passed"])
            self.assertGreater(report["summary"]["failures"], 0)

    def test_proxy_mode_rejects_missing_marker(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / "reproduced"
            self.proxy_fixture(root)
            (root / "data/proxy/.realistic_proxy_data").unlink()
            report = MODULE.Verifier(root, "proxy", (4,)).run()
            self.assertFalse(report["passed"])
            self.assertGreater(report["summary"]["failures"], 0)

    def test_proxy_mode_rejects_degenerate_chapter4_residence_dyads(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / "reproduced"
            self.proxy_fixture(root)
            manifest_path = root / "outputs/chapter4/manifests/prepared_data_manifest.json"
            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            manifest["network_arrays"]["dyadic_nonzero_counts"] = {
                "flatmates": 0,
                "blockmates": 0,
            }
            write_json(manifest_path, manifest)
            report = MODULE.Verifier(root, "proxy", (4,)).run()
            self.assertFalse(report["passed"])
            self.assertGreater(report["summary"]["failures"], 0)

    def test_real_mode_rejects_synthetic_chapter4_output(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / "reproduced"
            write(root / "data/raw/list_by_wave.RData")
            write(root / "outputs/chapter4/data/prepared_data_sets.RData")
            write_json(
                root / "outputs/chapter4/manifests/prepared_data_manifest.json",
                {
                    "entries": [],
                    "network_arrays": {
                        "dyadic_nonzero_counts": {"flatmates": 50, "blockmates": 200}
                    },
                },
            )
            write_json(
                root / "outputs/chapter4/logs/chapter4_qa_assertions.json",
                {"synthetic_marker_present": True, "overall_pass": True},
            )
            report = MODULE.Verifier(root, "real", (4,)).run()
            self.assertFalse(report["passed"])


if __name__ == "__main__":
    unittest.main()
