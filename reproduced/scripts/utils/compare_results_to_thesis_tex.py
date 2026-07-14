#!/usr/bin/env python3

from __future__ import annotations

import argparse
import csv
import json
import math
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Tuple


@dataclass(frozen=True)
class Chapter5Expectation:
    time_period: str
    term: str
    estimate: float
    std_error: float


@dataclass(frozen=True)
class SaomExpectation:
    thesis_effect: str
    estimate: float
    std_error: float
    p_value: float


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace")


def ensure_parent(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)


def strip_tex(text: str) -> str:
    text = text.strip()
    text = re.sub(r"\\gls\{([^}]+)\}", r"\1", text)
    text = re.sub(r"\\textbf\{([^}]+)\}", r"\1", text)
    text = re.sub(r"\\textit\{([^}]+)\}", r"\1", text)
    text = re.sub(r"\\label\{[^}]+\}", "", text)
    text = re.sub(r"\\caption\{[^}]+\}", "", text)
    text = text.replace("~", " ")
    text = re.sub(r"\s+", " ", text)
    return text.strip()


def parse_float(text: str) -> Optional[float]:
    if text is None:
        return None
    match = re.search(r"[-+]?\d+(?:\.\d+)?", text)
    if not match:
        return None
    try:
        return float(match.group(0))
    except ValueError:
        return None


def approx_equal(a: Optional[float], b: Optional[float], tol: float = 1e-9) -> bool:
    if a is None or b is None:
        return False
    return abs(a - b) <= tol


def parse_chapter5_both_model(tex: str) -> List[Chapter5Expectation]:
    expectations: List[Chapter5Expectation] = []
    for time_period in ("Time 1", "Time 2", "Time 3"):
        caption = rf"\\caption\{{{re.escape(time_period)} Results\}}"
        match = re.search(caption, tex)
        if not match:
            raise ValueError(f"Unable to locate subtable caption for {time_period}.")

        block = tex[match.end() :]
        end_idx = block.find(r"\end{subtable}")
        if end_idx == -1:
            raise ValueError(f"Unable to locate end of subtable for {time_period}.")
        block = block[:end_idx]

        def parse_term(term_label: str, term_key: str) -> Chapter5Expectation:
            line_match = re.search(rf"^{re.escape(term_label)}\s*&.*?&\s*(.*?)\\\\", block, flags=re.MULTILINE)
            if not line_match:
                raise ValueError(f"Unable to locate {term_label} row for {time_period}.")

            row = line_match.group(0)
            cols = [c.strip() for c in row.split("&")]
            if len(cols) < 5:
                raise ValueError(f"Unexpected column structure for {term_label} in {time_period}: {row}")
            both_col = cols[-1]
            coef_match = re.search(
                r"([-+]?\d+(?:\.\d+)?)\s*(?:\*+|\$\\dagger\$|\.)?\s*\(([-+]?\d+(?:\.\d+)?)\)",
                both_col,
            )
            if not coef_match:
                raise ValueError(f"Unable to parse coefficient/SE for {term_label} in {time_period}: {both_col}")

            estimate = float(coef_match.group(1))
            std_error = float(coef_match.group(2))
            return Chapter5Expectation(time_period=time_period, term=term_key, estimate=estimate, std_error=std_error)

        expectations.append(parse_term("Global-level misperception", "global_misperception"))
        expectations.append(parse_term("Peer-level misperception", "peer_misperception"))

    return expectations


def load_nam_summary(path: Path) -> Dict[Tuple[str, str], Dict[str, float]]:
    with path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        out: Dict[Tuple[str, str], Dict[str, float]] = {}
        for row in reader:
            key = (row["time_period"], row["term"])
            out[key] = {
                "estimate": float(row["estimate"]),
                "std_error": float(row["std_error"]),
            }
        return out


def compare_chapter5(tex_path: Path, nam_summary_path: Path, output_path: Path) -> None:
    tex = read_text(tex_path)
    expectations = parse_chapter5_both_model(tex)
    actual = load_nam_summary(nam_summary_path)

    rows: List[Dict[str, Any]] = []
    for exp in expectations:
        key = (exp.time_period, exp.term)
        act = actual.get(key)
        if act is None:
            raise ValueError(f"Missing {key} in {nam_summary_path}")

        estimate_diff = act["estimate"] - exp.estimate
        se_diff = act["std_error"] - exp.std_error
        rows.append(
            {
                "time_period": exp.time_period,
                "term": exp.term,
                "thesis_estimate": exp.estimate,
                "thesis_std_error": exp.std_error,
                "reproduced_estimate": act["estimate"],
                "reproduced_std_error": act["std_error"],
                "estimate_difference": estimate_diff,
                "std_error_difference": se_diff,
            }
        )

    ensure_parent(output_path)
    with output_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def parse_chapter7_saom_table(tex: str) -> Tuple[List[SaomExpectation], Optional[float]]:
    start = tex.find(r"\begin{longtable")
    if start == -1:
        raise ValueError("Unable to locate SAOM_results longtable start.")
    end = tex.find(r"\end{longtable}", start)
    if end == -1:
        raise ValueError("Unable to locate SAOM_results longtable end.")

    block = tex[start:end]
    row_pattern = re.compile(r"^(.*?)\s*&\s*(.*?)\s*&\s*(.*?)\s*&\s*(.*?)\s*\\\\\s*$", re.MULTILINE)

    expectations: List[SaomExpectation] = []
    thesis_tconv: Optional[float] = None

    for match in row_pattern.finditer(block):
        effect_raw, beta_raw, se_raw, p_raw = match.groups()
        effect = strip_tex(effect_raw)
        beta_text = strip_tex(beta_raw)
        se_text = strip_tex(se_raw)
        p_text = strip_tex(p_raw)

        if not effect or effect.lower() in {"effects"}:
            continue

        if effect.startswith("Overall max.") or effect.startswith("Overall max"):
            thesis_tconv = parse_float(beta_text)
            continue

        estimate = parse_float(beta_text)
        std_error = parse_float(se_text)
        p_value = parse_float(p_text)
        if estimate is None or std_error is None or p_value is None:
            continue

        expectations.append(
            SaomExpectation(
                thesis_effect=effect,
                estimate=estimate,
                std_error=std_error,
                p_value=p_value,
            )
        )

    if not expectations:
        raise ValueError("No coefficient rows parsed from SAOM_results table.")

    return expectations, thesis_tconv


def load_saom_coefficients(path: Path) -> Dict[str, Dict[str, float]]:
    with path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        out: Dict[str, Dict[str, float]] = {}
        for row in reader:
            label = row.get("label") or row.get("effect")
            if not label:
                continue
            out[label] = {
                "estimate": float(row["estimate"]),
                "std_error": float(row["std_error"]),
                "p_value": float(row["p_value"]),
            }
        return out


def load_saom_run_log_tconv(path: Path) -> Optional[float]:
    if not path.exists():
        return None
    payload = json.loads(read_text(path))
    value = payload.get("tconv_max")
    if value is None:
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def compare_chapter7(
    tex_path: Path,
    coefficients_path: Path,
    run_log_path: Path,
    output_path: Path,
) -> None:
    tex = read_text(tex_path)
    expectations, thesis_tconv = parse_chapter7_saom_table(tex)
    actual = load_saom_coefficients(coefficients_path)

    label_map: Dict[str, str] = {
        "Constant friendship rate (period 1)": "constant SAOM_friendship rate (period 1)",
        "Constant friendship rate (period 2)": "constant SAOM_friendship rate (period 2)",
        "Constant friendship rate (period 3)": "constant SAOM_friendship rate (period 3)",
        "Outdegree (density)": "outdegree (density)",
        "Reciprocity": "reciprocity",
        "Transitive triplets": "transitive triplets",
        "Transitive reciprocated triplets": "transitive recipr. triplets",
        "Indegree - popularity (sqrt)": "indegree - popularity (sqrt)",
        "Outdegree - popularity (sqrt)": "outdegree - popularity (sqrt)",
        "Outdegree - activity (sqrt)": "outdegree - activity (sqrt)",
        "Flatmates": "flatmates",
        "Blockmates": "blockmates",
        "If majority alter": "majority_status alter",
        "If majority ego": "majority_status ego",
        "If majority similarity": "majority_status similarity",
        "Sex alter": "sex alter",
        "Sex ego": "sex ego",
        "Sex similarity": "sex similarity",
        "auditc score alter": "SAOM_behaviour alter",
        "auditc score ego": "SAOM_behaviour ego",
        "auditc score similarity": "SAOM_behaviour similarity",
        "Rate auditc score (period 1)": "rate SAOM_behaviour (period 1)",
        "Rate auditc score (period 2)": "rate SAOM_behaviour (period 2)",
        "Rate auditc score (period 3)": "rate SAOM_behaviour (period 3)",
        "auditc score linear shape": "SAOM_behaviour linear shape",
        "auditc score quadratic shape": "SAOM_behaviour quadratic shape",
        "auditc score average similarity": "SAOM_behaviour average similarity",
        "auditc score indegree": "SAOM_behaviour indegree",
        "auditc score outdegree": "SAOM_behaviour outdegree",
        "auditc score: effect from if majority": "SAOM_behaviour: effect from majority_status",
        "auditc score: effect from sex": "SAOM_behaviour: effect from sex",
    }

    rows: List[Dict[str, Any]] = []
    for exp in expectations:
        mapped_label = label_map.get(exp.thesis_effect)
        if mapped_label is None:
            continue
        act = actual.get(mapped_label)
        if act is None:
            raise ValueError(f"Missing reproduced coefficient '{mapped_label}' in {coefficients_path}")

        rows.append(
            {
                "thesis_effect": exp.thesis_effect,
                "reproduced_label": mapped_label,
                "thesis_estimate": exp.estimate,
                "reproduced_estimate": act["estimate"],
                "estimate_difference": act["estimate"] - exp.estimate,
                "thesis_std_error": exp.std_error,
                "reproduced_std_error": act["std_error"],
                "std_error_difference": act["std_error"] - exp.std_error,
                "thesis_p_value": exp.p_value,
                "reproduced_p_value": act["p_value"],
                "p_value_difference": act["p_value"] - exp.p_value,
            }
        )

    run_tconv = load_saom_run_log_tconv(run_log_path)
    if thesis_tconv is not None or run_tconv is not None:
        rows.append(
            {
                "thesis_effect": "Overall max. convergence ratio",
                "reproduced_label": "tconv_max",
                "thesis_estimate": thesis_tconv,
                "reproduced_estimate": run_tconv,
                "estimate_difference": None if thesis_tconv is None or run_tconv is None else run_tconv - thesis_tconv,
                "thesis_std_error": None,
                "reproduced_std_error": None,
                "std_error_difference": None,
                "thesis_p_value": None,
                "reproduced_p_value": None,
                "p_value_difference": None,
            }
        )

    ensure_parent(output_path)
    with output_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def main() -> int:
    parser = argparse.ArgumentParser(description="Compare reproduced outputs against thesis TeX tables.")
    parser.add_argument(
        "--chapter5-tex",
        default="shang_thesis_may/chapters/chapter5.tex",
        help="Path to thesis chapter5.tex",
    )
    parser.add_argument(
        "--chapter7-tex",
        default="shang_thesis_may/chapters/chapter7.tex",
        help="Path to thesis chapter7.tex",
    )
    parser.add_argument(
        "--nam-summary",
        default="reproduced/outputs/chapter5/tables/nam_summary.csv",
        help="Path to reproduced NAM summary CSV",
    )
    parser.add_argument(
        "--saom-coefficients",
        default="reproduced/outputs/chapter7/tables/saom_coefficients_base.csv",
        help="Path to reproduced SAOM coefficients CSV",
    )
    parser.add_argument(
        "--saom-run-log",
        default="reproduced/outputs/chapter7/logs/saom_run_base.json",
        help="Path to reproduced SAOM run log JSON",
    )
    parser.add_argument(
        "--out-chapter5",
        default="reproduced/outputs/chapter5/validations/nam_diff_vs_thesis_tex.csv",
        help="Output path for Chapter 5 diff CSV",
    )
    parser.add_argument(
        "--out-chapter7",
        default="reproduced/outputs/chapter7/validations/saom_diff_vs_thesis_tex.csv",
        help="Output path for Chapter 7 diff CSV",
    )

    args = parser.parse_args()

    compare_chapter5(Path(args.chapter5_tex), Path(args.nam_summary), Path(args.out_chapter5))
    compare_chapter7(
        Path(args.chapter7_tex),
        Path(args.saom_coefficients),
        Path(args.saom_run_log),
        Path(args.out_chapter7),
    )

    print(f"[chapter5] wrote {args.out_chapter5}")
    print(f"[chapter7] wrote {args.out_chapter7}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
