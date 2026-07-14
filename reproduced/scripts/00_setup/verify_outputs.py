#!/usr/bin/env python3
"""Verify SAND outputs without conflating proxy and real-data evidence.

Proxy mode checks that the public workflow executed and produced structurally
valid artifacts. It records empirical coefficient or convergence differences
as warnings because synthetic data are not expected to reproduce thesis
results. Real mode treats the recorded chapter benchmarks and convergence
criteria as release-blocking checks.
"""

from __future__ import annotations

import argparse
import csv
import json
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Iterable


VALID_CHAPTERS = (4, 5, 6, 7)


def _load_json(path: Path) -> Any:
    with path.open(encoding="utf-8") as handle:
        return json.load(handle)


def _csv_rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def _resolve_recorded_path(value: Any, repro_root: Path) -> Path | None:
    if not isinstance(value, str) or not value.strip():
        return None
    candidate = Path(value)
    if candidate.is_absolute():
        return candidate
    repo_root = repro_root.parent
    for base in (repo_root, repro_root):
        resolved = base / candidate
        if resolved.exists():
            return resolved
    return repo_root / candidate


class Verifier:
    def __init__(self, repro_root: Path, mode: str, chapters: Iterable[int]) -> None:
        self.root = repro_root.resolve()
        self.mode = mode
        self.chapters = tuple(chapters)
        self.checks: list[dict[str, Any]] = []

    def add(self, check_id: str, status: str, detail: str, evidence: str | None = None) -> None:
        entry: dict[str, Any] = {"id": check_id, "status": status, "detail": detail}
        if evidence:
            entry["evidence"] = evidence
        self.checks.append(entry)

    def require_file(self, check_id: str, rel: str, *, min_bytes: int = 1) -> Path | None:
        path = self.root / rel
        if not path.is_file():
            self.add(check_id, "fail", "Required artifact is missing.", rel)
            return None
        size = path.stat().st_size
        if size < min_bytes:
            self.add(check_id, "fail", f"Artifact is smaller than {min_bytes} byte(s).", rel)
            return None
        self.add(check_id, "pass", f"Artifact exists ({size} bytes).", rel)
        return path

    def require_json(self, check_id: str, rel: str) -> tuple[Path | None, Any | None]:
        path = self.require_file(check_id, rel)
        if path is None:
            return None, None
        try:
            return path, _load_json(path)
        except (OSError, json.JSONDecodeError) as exc:
            self.checks[-1] = {
                "id": check_id,
                "status": "fail",
                "detail": f"Artifact is not valid JSON: {exc}",
                "evidence": rel,
            }
            return path, None

    def verify_data_boundary(self) -> None:
        if self.mode == "proxy":
            self.require_file("data.proxy_bundle", "data/proxy/list_by_wave.RData")
            self.require_file("data.proxy_marker", "data/proxy/.realistic_proxy_data", min_bytes=0)
        else:
            self.require_file("data.real_bundle", "data/raw/list_by_wave.RData")
            raw_markers = [
                self.root / "data/raw/.realistic_proxy_data",
                self.root / "data/raw/.chapter4_synthetic",
            ]
            present = [p.name for p in raw_markers if p.exists()]
            if present:
                self.add("data.real_no_proxy_marker", "fail", f"Proxy marker(s) found in real-data directory: {', '.join(present)}")
            else:
                self.add("data.real_no_proxy_marker", "pass", "No proxy markers found in the protected real-data directory.")

    def verify_chapter4(self) -> None:
        self.require_file("chapter4.prepared_data", "outputs/chapter4/data/prepared_data_sets.RData")
        _, manifest = self.require_json("chapter4.manifest", "outputs/chapter4/manifests/prepared_data_manifest.json")
        if isinstance(manifest, dict):
            dyadic_counts = ((manifest.get("network_arrays") or {}).get("dyadic_nonzero_counts"))
            required_dyads = ("flatmates", "blockmates")
            dyads_exercised = isinstance(dyadic_counts, dict) and all(
                isinstance(dyadic_counts.get(name), (int, float)) and dyadic_counts[name] > 0
                for name in required_dyads
            )
            self.add(
                "chapter4.dyadic_covariates",
                "pass" if dyads_exercised else "fail",
                (
                    f"Chapter 4 residence dyads are non-degenerate ({dyadic_counts})."
                    if dyads_exercised
                    else f"Expected non-zero flatmate and blockmate dyads; recorded {dyadic_counts}."
                ),
                "outputs/chapter4/manifests/prepared_data_manifest.json",
            )
        _, qa = self.require_json("chapter4.qa", "outputs/chapter4/logs/chapter4_qa_assertions.json")
        if not isinstance(qa, dict):
            return
        synthetic = bool(qa.get("synthetic_marker_present"))
        overall = bool(qa.get("overall_pass"))
        expected_synthetic = self.mode == "proxy"
        if synthetic != expected_synthetic:
            self.add(
                "chapter4.data_mode",
                "fail",
                f"QA recorded synthetic_marker_present={synthetic}; expected {expected_synthetic} for {self.mode} mode.",
                "outputs/chapter4/logs/chapter4_qa_assertions.json",
            )
        else:
            self.add("chapter4.data_mode", "pass", f"QA artifact records the expected {self.mode} data boundary.")
        self.add(
            "chapter4.qa_status",
            "pass" if overall else "fail",
            "Chapter 4 QA assertions passed." if overall else "One or more Chapter 4 QA assertions failed.",
            "outputs/chapter4/logs/chapter4_qa_assertions.json",
        )

    def verify_chapter5(self) -> None:
        summary = self.require_file("chapter5.summary", "outputs/chapter5/tables/nam_summary.csv")
        if summary is not None:
            try:
                rows = _csv_rows(summary)
                required = {"time_period", "estimate"}
                missing = required - set(rows[0]) if rows else required
                has_term = bool(rows) and ("term" in rows[0] or "term_raw" in rows[0])
                if not has_term:
                    missing.add("term or term_raw")
                if not rows or missing:
                    self.add("chapter5.summary_schema", "fail", f"NAM summary has no rows or lacks columns: {', '.join(sorted(missing))}.")
                else:
                    self.add("chapter5.summary_schema", "pass", f"NAM summary contains {len(rows)} coefficient rows.")
            except (OSError, csv.Error) as exc:
                self.add("chapter5.summary_schema", "fail", f"Unable to read NAM summary: {exc}")

        _, validation = self.require_json("chapter5.benchmarks", "outputs/chapter5/logs/nam_validation.json")
        if not isinstance(validation, dict) or not isinstance(validation.get("results"), list):
            return
        results = validation["results"]
        failures = sum(not bool(row.get("passed")) for row in results if isinstance(row, dict))
        if self.mode == "proxy":
            status = "warn" if failures else "pass"
            detail = (
                f"{failures}/{len(results)} thesis-coefficient checks differ, as expected for synthetic inputs."
                if failures
                else f"All {len(results)} recorded coefficient checks happen to be within tolerance; this is not empirical validation."
            )
        else:
            status = "fail" if failures else "pass"
            detail = f"{failures}/{len(results)} real-data thesis-coefficient checks failed."
        self.add("chapter5.benchmark_status", status, detail, "outputs/chapter5/logs/nam_validation.json")

    def verify_chapter6(self) -> None:
        self.require_file("chapter6.coefficients", "outputs/chapter6/tables/injunctive_nam_coefficients.csv")
        _, validation = self.require_json(
            "chapter6.benchmarks", "outputs/chapter6/manifests/injunctive_nam_validation.json"
        )
        if not isinstance(validation, dict):
            return
        synthetic = bool(validation.get("synthetic_data"))
        if synthetic != (self.mode == "proxy"):
            self.add("chapter6.data_mode", "fail", f"Validation records synthetic_data={synthetic}, inconsistent with {self.mode} mode.")
        else:
            self.add("chapter6.data_mode", "pass", f"Validation records the expected {self.mode} data boundary.")
        passed = validation.get("status") == "passed"
        if self.mode == "proxy":
            status = "pass" if passed else "warn"
            detail = (
                "Synthetic coefficients happen to match recorded thesis tolerances; this is not empirical validation."
                if passed
                else "Synthetic coefficients differ from thesis benchmarks, as expected for proxy inputs."
            )
        else:
            status = "pass" if passed else "fail"
            detail = "Real-data Chapter 6 benchmarks passed." if passed else "Real-data Chapter 6 benchmarks failed."
        self.add("chapter6.benchmark_status", status, detail, "outputs/chapter6/manifests/injunctive_nam_validation.json")

    def verify_chapter7(self) -> None:
        self.require_file("chapter7.network_arrays", "outputs/chapter4/data/network_arrays.rds")
        self.require_json("chapter7.input_manifest", "outputs/chapter7/manifests/saom_data_manifest.json")
        _, run = self.require_json("chapter7.run_log", "outputs/chapter7/logs/saom_run_base.json")
        if not isinstance(run, dict):
            return
        placeholder = bool((run.get("inputs") or {}).get("placeholder", run.get("placeholder", False)))
        self.add(
            "chapter7.no_placeholder",
            "fail" if placeholder else "pass",
            "Chapter 7 used placeholder inputs." if placeholder else "Chapter 7 used non-placeholder inputs.",
        )
        dyadic_counts = run.get("dyadic_nonzero_counts")
        required_dyads = ("flatmates", "blockmates")
        dyads_exercised = isinstance(dyadic_counts, dict) and all(
            isinstance(dyadic_counts.get(name), (int, float)) and dyadic_counts[name] > 0
            for name in required_dyads
        )
        self.add(
            "chapter7.dyadic_covariates",
            "pass" if dyads_exercised else "fail",
            (
                f"Residence dyadic covariates are non-degenerate ({dyadic_counts})."
                if dyads_exercised
                else f"Expected non-zero flatmate and blockmate covariates; recorded {dyadic_counts}."
            ),
            "outputs/chapter7/logs/saom_run_base.json",
        )
        fit_path = _resolve_recorded_path(run.get("fit_path"), self.root)
        if fit_path is None or not fit_path.is_file():
            self.add("chapter7.fit", "fail", "The recorded SAOM fit is missing.")
        else:
            self.add("chapter7.fit", "pass", f"SAOM fit exists ({fit_path.stat().st_size} bytes).", "outputs/chapter7/cache/base_fit.RData")

        converged = run.get("status") == "converged" and not bool(run.get("diagnostic_failures"))
        tconv = run.get("tconv_max")
        tolerance = run.get("convergence_tolerance")
        if self.mode == "proxy":
            if converged:
                self.add("chapter7.convergence", "pass", f"Synthetic SAOM converged (tconv.max={tconv}, tolerance={tolerance}).")
            else:
                self.add(
                    "chapter7.convergence",
                    "warn",
                    f"Synthetic SAOM did not meet empirical convergence/target criteria (status={run.get('status')}, tconv.max={tconv}, tolerance={tolerance}).",
                    "outputs/chapter7/logs/saom_run_base.json",
                )
        else:
            self.add(
                "chapter7.convergence",
                "pass" if converged else "fail",
                f"Real-data SAOM status={run.get('status')}, tconv.max={tconv}, tolerance={tolerance}.",
                "outputs/chapter7/logs/saom_run_base.json",
            )
            _, coefficient_validation = self.require_json(
                "chapter7.coefficient_validation",
                "outputs/chapter7/validations/saom_coefficient_validation_base.json",
            )
            if isinstance(coefficient_validation, dict):
                passed = coefficient_validation.get("status") == "passed" and not coefficient_validation.get("placeholder")
                self.add(
                    "chapter7.benchmark_status",
                    "pass" if passed else "fail",
                    f"Real-data SAOM coefficient validation status={coefficient_validation.get('status')}.",
                )

    def run(self) -> dict[str, Any]:
        self.verify_data_boundary()
        for chapter in self.chapters:
            getattr(self, f"verify_chapter{chapter}")()
        failures = sum(check["status"] == "fail" for check in self.checks)
        warnings = sum(check["status"] == "warn" for check in self.checks)
        return {
            "schema_version": 1,
            "generated_at": datetime.now(timezone.utc).isoformat(),
            "verification_type": f"{self.mode}_structural" if self.mode == "proxy" else "real_empirical",
            "data_mode": self.mode,
            "chapters": list(self.chapters),
            "passed": failures == 0,
            "scientific_validation_passed": failures == 0 if self.mode == "real" else None,
            "summary": {"checks": len(self.checks), "failures": failures, "warnings": warnings},
            "interpretation": (
                "Proxy verification demonstrates execution and artifact structure only; it does not reproduce empirical findings."
                if self.mode == "proxy"
                else "Real verification requires chapter benchmarks and Chapter 7 diagnostics to pass on protected inputs."
            ),
            "checks": self.checks,
        }


def _parse_chapters(raw: str) -> tuple[int, ...]:
    try:
        chapters = tuple(dict.fromkeys(int(value.strip()) for value in raw.split(",") if value.strip()))
    except ValueError as exc:
        raise argparse.ArgumentTypeError("Chapters must be comma-separated integers.") from exc
    invalid = [chapter for chapter in chapters if chapter not in VALID_CHAPTERS]
    if not chapters or invalid:
        raise argparse.ArgumentTypeError(f"Choose one or more chapters from {VALID_CHAPTERS}.")
    return chapters


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--mode", choices=("proxy", "real"), required=True)
    parser.add_argument("--chapters", type=_parse_chapters, default=VALID_CHAPTERS)
    parser.add_argument("--repro-root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    report = Verifier(args.repro_root, args.mode, args.chapters).run()
    output = args.output or (args.repro_root / "outputs" / "verify" / f"{args.mode}_report.json")
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")

    summary = report["summary"]
    print(
        f"[{args.mode}-verify] {'PASS' if report['passed'] else 'FAIL'}: "
        f"{summary['checks']} checks, {summary['failures']} failures, {summary['warnings']} warnings."
    )
    print(f"[{args.mode}-verify] Report: {output}")
    return 0 if report["passed"] else 1


if __name__ == "__main__":
    sys.exit(main())
