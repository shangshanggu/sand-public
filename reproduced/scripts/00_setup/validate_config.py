#!/usr/bin/env python3
"""Validate the thesis configuration schema defined in ``reproduced/config/thesis.yml``.

This script loads the canonical configuration file and checks that all
required sections, keys, and value types
are present. The validator intentionally avoids third-party dependencies so it
can run in freshly bootstrapped environments that only provide the Python
standard library.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any, Dict, List, Sequence

REPO_ROOT = Path(__file__).resolve().parents[3]
REPRO_ROOT = REPO_ROOT / "reproduced"

for path in (REPO_ROOT, REPRO_ROOT):
    if str(path) not in sys.path:
        sys.path.insert(0, str(path))

from scripts.utils.yaml_loader import load_yaml_minimal

DEFAULT_CONFIG_PATH = REPRO_ROOT / "config" / "thesis.yml"




class ValidationError(Exception):
    """Raised when validation fails."""


def _expect_keys(
    obj: Dict[str, Any], required: Sequence[str], *, path: str, errors: List[str]
) -> None:
    for key in required:
        if key not in obj:
            errors.append(f"{path} missing required key '{key}'")


def _expect_type(value: Any, expected_type: type | tuple[type, ...], *, path: str, errors: List[str]) -> None:
    if not isinstance(value, expected_type):
        type_name = (
            " or ".join(sorted({t.__name__ for t in expected_type}))
            if isinstance(expected_type, tuple)
            else expected_type.__name__
        )
        errors.append(f"{path} must be of type {type_name}, found {type(value).__name__}")


def validate_config(config: Dict[str, Any]) -> List[str]:
    errors: List[str] = []

    if not isinstance(config, dict):
        errors.append("Root of configuration must be a mapping")
        return errors

    _expect_keys(config, ["project", "data", "chapters", "rsiena"], path="root", errors=errors)

    project = config.get("project", {})
    if isinstance(project, dict):
        _expect_keys(
            project,
            ["name", "version", "timezone", "paths", "environment"],
            path="project",
            errors=errors,
        )
        paths = project.get("paths", {})
        if isinstance(paths, dict):
            _expect_keys(
                paths,
                [
                    "repository_root",
                    "config_dir",
                    "raw_data_dir",
                    "processed_data_dir",
                    "intermediate_data_dir",
                    "external_data_dir",
                    "logs_dir",
                    "figures_dir",
                    "tables_dir",
                    "thesis_assets_dir",
                    "thesis_figures_dir",
                    "thesis_tables_dir",
                    "thesis_manifests_dir",
                    "thesis_tex_dir",
                    "thesis_pdf_dir",
                    "thesis_md_dir",
                ],
                path="project.paths",
                errors=errors,
            )
            for key, value in paths.items():
                _expect_type(value, str, path=f"project.paths.{key}", errors=errors)
        else:
            errors.append("project.paths must be a mapping")

        environment = project.get("environment", {})
        if isinstance(environment, dict):
            _expect_keys(
                environment,
                ["conda_env", "renv_lock", "environment_manifest", "docker_image"],
                path="project.environment",
                errors=errors,
            )
            for key, value in environment.items():
                _expect_type(value, str, path=f"project.environment.{key}", errors=errors)
        else:
            errors.append("project.environment must be a mapping")
    else:
        errors.append("project must be a mapping")

    data_section = config.get("data", {})
    if isinstance(data_section, dict):
        _expect_keys(
            data_section,
            ["raw_files", "metadata_manifest", "checksum_log", "allow_missing_raw"],
            path="data",
            errors=errors,
        )
        raw_files = data_section.get("raw_files", {})
        if isinstance(raw_files, dict):
            _expect_keys(
                raw_files,
                ["participants", "outcomes"],
                path="data.raw_files",
                errors=errors,
            )
            for key, value in raw_files.items():
                _expect_type(value, str, path=f"data.raw_files.{key}", errors=errors)
        else:
            errors.append("data.raw_files must be a mapping")
        if "metadata_manifest" in data_section:
            _expect_type(
                data_section["metadata_manifest"],
                str,
                path="data.metadata_manifest",
                errors=errors,
            )
        if "checksum_log" in data_section:
            _expect_type(
                data_section["checksum_log"],
                str,
                path="data.checksum_log",
                errors=errors,
            )
        if "allow_missing_raw" in data_section:
            _expect_type(
                data_section["allow_missing_raw"],
                bool,
                path="data.allow_missing_raw",
                errors=errors,
            )
        if "mode" in data_section:
            _expect_type(
                data_section["mode"],
                str,
                path="data.mode",
                errors=errors,
            )
            if isinstance(data_section.get("mode"), str):
                mode = data_section["mode"].strip().lower()
                if mode and mode not in ("real", "proxy"):
                    errors.append("data.mode must be 'real' or 'proxy'")
        if "proxy_dir" in data_section:
            _expect_type(
                data_section["proxy_dir"],
                str,
                path="data.proxy_dir",
                errors=errors,
            )
    else:
        errors.append("data must be a mapping")

    chapters = config.get("chapters", {})
    if isinstance(chapters, dict):
        expected_chapters = {
            "chapter4_data_collection": {
                "required": [
                    "enabled",
                    "description",
                    "scripts_dir",
                    "outputs_dir",
                    "required_inputs",
                    "qa_expectations",
                    "spec_parameters",
                ]
            },
            "chapter5_descriptive_norms": {
                "required": [
                    "enabled",
                    "description",
                    "scripts_dir",
                    "outputs_dir",
                    "required_inputs",
                    "reference_results",
                ]
            },
            "chapter6_injunctive_norms": {
                "required": [
                    "enabled",
                    "description",
                    "scripts_dir",
                    "outputs_dir",
                    "required_inputs",
                    "longitudinal_data",
                    "scenarios",
                ]
            },
            "chapter7_saom": {
                "required": [
                    "enabled",
                    "description",
                    "scripts_dir",
                    "outputs_dir",
                    "cache_dir",
                    "required_inputs",
                    "target_coefficients",
                ]
            },
            "chapter8_interventions": {
                "required": [
                    "enabled",
                    "description",
                    "scripts_dir",
                    "outputs_dir",
                    "scenario_manifest",
                    "default_batches",
                ]
            },
        }
        for chapter_key, info in expected_chapters.items():
            chapter_obj = chapters.get(chapter_key)
            if not isinstance(chapter_obj, dict):
                errors.append(f"chapters.{chapter_key} must be a mapping")
                continue
            _expect_keys(
                chapter_obj,
                info["required"],
                path=f"chapters.{chapter_key}",
                errors=errors,
            )
            _expect_type(
                chapter_obj.get("enabled"),
                bool,
                path=f"chapters.{chapter_key}.enabled",
                errors=errors,
            )
            for string_field in [
                "description",
                "scripts_dir",
                "outputs_dir",
                "cache_dir",
                "reference_results",
                "longitudinal_data",
                "scenario_manifest",
            ]:
                if string_field in chapter_obj:
                    _expect_type(
                        chapter_obj[string_field],
                        str,
                        path=f"chapters.{chapter_key}.{string_field}",
                        errors=errors,
                    )
            if "required_inputs" in chapter_obj:
                inputs = chapter_obj["required_inputs"]
                if not isinstance(inputs, list) or not inputs:
                    errors.append(
                        f"chapters.{chapter_key}.required_inputs must be a non-empty list"
                    )
                else:
                    for idx, item in enumerate(inputs):
                        _expect_type(
                            item,
                            str,
                            path=f"chapters.{chapter_key}.required_inputs[{idx}]",
                            errors=errors,
                        )
            if "qa_expectations" in chapter_obj:
                qa = chapter_obj["qa_expectations"]
                if isinstance(qa, dict):
                    for k, v in qa.items():
                        if not isinstance(v, (int, float)):
                            errors.append(
                                f"chapters.{chapter_key}.qa_expectations.{k} must be numeric"
                            )
                else:
                    errors.append(
                        f"chapters.{chapter_key}.qa_expectations must be a mapping"
                    )
            if chapter_key == "chapter4_data_collection":
                spec_params = chapter_obj.get("spec_parameters")
                if isinstance(spec_params, dict):
                    list_fields = [
                        "imputation_method",
                        "include_perception",
                        "model_type",
                        "time_periods",
                        "reference_group",
                        "typical_definition",
                        "separate_or_together",
                    ]
                    for field in list_fields:
                        if field not in spec_params:
                            errors.append(
                                f"chapters.{chapter_key}.spec_parameters missing required key '{field}'"
                            )
                            continue
                        values = spec_params[field]
                        if not isinstance(values, list) or not values:
                            errors.append(
                                f"chapters.{chapter_key}.spec_parameters.{field} must be a non-empty list"
                            )
                        else:
                            for idx, item in enumerate(values):
                                _expect_type(
                                    item,
                                    str,
                                    path=(
                                        f"chapters.{chapter_key}.spec_parameters.{field}[{idx}]"
                                    ),
                                    errors=errors,
                                )
                    measures = spec_params.get("measures")
                    if isinstance(measures, dict):
                        for measure_key, measure_value in measures.items():
                            if not isinstance(measure_value, dict):
                                errors.append(
                                    f"chapters.chapter4_data_collection.spec_parameters.measures.{measure_key} must be a mapping"
                                )
                                continue
                            for subfield in ("misperceptions", "outcomes"):
                                if subfield not in measure_value:
                                    errors.append(
                                        f"chapters.chapter4_data_collection.spec_parameters.measures.{measure_key} missing '{subfield}'"
                                    )
                                    continue
                                entries = measure_value[subfield]
                                if not isinstance(entries, list) or not entries:
                                    errors.append(
                                        f"chapters.chapter4_data_collection.spec_parameters.measures.{measure_key}.{subfield} must be a non-empty list"
                                    )
                                else:
                                    for idx, entry in enumerate(entries):
                                        _expect_type(
                                            entry,
                                            str,
                                            path=(
                                                f"chapters.chapter4_data_collection.spec_parameters.measures.{measure_key}.{subfield}[{idx}]"
                                            ),
                                            errors=errors,
                                        )
                            mis = measure_value.get("misperceptions")
                            outs = measure_value.get("outcomes")
                            if isinstance(mis, list) and isinstance(outs, list) and len(mis) != len(outs):
                                errors.append(
                                    f"chapters.chapter4_data_collection.spec_parameters.measures.{measure_key} misperceptions/outcomes must have matching lengths"
                                )
                    else:
                        errors.append(
                            "chapters.chapter4_data_collection.spec_parameters.measures must be a mapping"
                        )
                else:
                    errors.append(
                        "chapters.chapter4_data_collection.spec_parameters must be a mapping"
                    )
            if chapter_key == "chapter6_injunctive_norms":
                scenarios = chapter_obj.get("scenarios")
                if not isinstance(scenarios, dict) or not scenarios:
                    errors.append(
                        "chapters.chapter6_injunctive_norms.scenarios must be a non-empty mapping"
                    )
                else:
                    for scenario_name, scenario in scenarios.items():
                        scenario_path = (
                            f"chapters.chapter6_injunctive_norms.scenarios.{scenario_name}"
                        )
                        if not isinstance(scenario, dict):
                            errors.append(f"{scenario_path} must be a mapping")
                            continue
                        required_fields = [
                            "label",
                            "imputation_method",
                            "typical_definition",
                            "approval_column",
                            "outcome_column",
                        ]
                        for field in required_fields:
                            if field not in scenario:
                                errors.append(
                                    f"{scenario_path} missing required key '{field}'"
                                )
                                continue
                            _expect_type(
                                scenario[field],
                                str,
                                path=f"{scenario_path}.{field}",
                                errors=errors,
                            )
                        if "key" in scenario:
                            _expect_type(
                                scenario["key"],
                                str,
                                path=f"{scenario_path}.key",
                                errors=errors,
                            )
                        for optional_field in [
                            "misperception_peer_column",
                            "misperception_global_column",
                        ]:
                            if optional_field in scenario and scenario[optional_field] is not None:
                                _expect_type(
                                    scenario[optional_field],
                                    str,
                                    path=f"{scenario_path}.{optional_field}",
                                    errors=errors,
                                )
                        waves = scenario.get("waves")
                        if waves is not None:
                            if not isinstance(waves, list) or not waves:
                                errors.append(
                                    f"{scenario_path}.waves must be a non-empty list if provided"
                                )
                            else:
                                for wave_idx, wave in enumerate(waves):
                                    _expect_type(
                                        wave,
                                        str,
                                        path=f"{scenario_path}.waves[{wave_idx}]",
                                        errors=errors,
                                    )
            if "target_coefficients" in chapter_obj:
                targets = chapter_obj["target_coefficients"]
                if isinstance(targets, dict):
                    for coef_key, coef_value in targets.items():
                        if not isinstance(coef_value, (int, float)):
                            errors.append(
                                f"chapters.{chapter_key}.target_coefficients.{coef_key} must be numeric"
                            )
                else:
                    errors.append(
                        f"chapters.{chapter_key}.target_coefficients must be a mapping"
                    )
            if "default_batches" in chapter_obj:
                _expect_type(
                    chapter_obj["default_batches"],
                    int,
                    path=f"chapters.{chapter_key}.default_batches",
                    errors=errors,
                )
    else:
        errors.append("chapters must be a mapping")

    thesis_section = config.get("thesis", {})
    if isinstance(thesis_section, dict):
        packaging = thesis_section.get("packaging")
        if isinstance(packaging, dict):
            _expect_keys(
                packaging,
                [
                    "pack_script",
                    "aggregate_root",
                    "tables_dir",
                    "figures_dir",
                    "manifests_dir",
                    "tables_manifest",
                    "figures_manifest",
                    "summary_manifest",
                    "include_chapters",
                    "asset_patterns",
                ],
                path="thesis.packaging",
                errors=errors,
            )
            for field in [
                "pack_script",
                "aggregate_root",
                "tables_dir",
                "figures_dir",
                "manifests_dir",
                "tables_manifest",
                "figures_manifest",
                "summary_manifest",
            ]:
                if field in packaging:
                    _expect_type(
                        packaging[field],
                        str,
                        path=f"thesis.packaging.{field}",
                        errors=errors,
                    )
            include_chapters = packaging.get("include_chapters")
            if isinstance(include_chapters, list) and include_chapters:
                for idx, chapter in enumerate(include_chapters):
                    _expect_type(
                        chapter,
                        str,
                        path=f"thesis.packaging.include_chapters[{idx}]",
                        errors=errors,
                    )
            else:
                errors.append(
                    "thesis.packaging.include_chapters must be a non-empty list of chapter identifiers"
                )
            asset_patterns = packaging.get("asset_patterns")
            if isinstance(asset_patterns, dict) and asset_patterns:
                for asset_type, patterns in asset_patterns.items():
                    if not isinstance(patterns, list) or not patterns:
                        errors.append(
                            f"thesis.packaging.asset_patterns.{asset_type} must be a non-empty list"
                        )
                        continue
                    for idx, pattern in enumerate(patterns):
                        _expect_type(
                            pattern,
                            str,
                            path=(
                                f"thesis.packaging.asset_patterns.{asset_type}[{idx}]"
                            ),
                            errors=errors,
                        )
            else:
                errors.append(
                    "thesis.packaging.asset_patterns must be a non-empty mapping"
                )
        else:
            errors.append("thesis.packaging must be a mapping")

        build_section = thesis_section.get("build")
        if isinstance(build_section, dict):
            _expect_keys(
                build_section,
                ["render_script", "project_dir", "source_file", "default_formats"],
                path="thesis.build",
                errors=errors,
            )
            for field in ["render_script", "project_dir", "source_file"]:
                if field in build_section:
                    _expect_type(
                        build_section[field],
                        str,
                        path=f"thesis.build.{field}",
                        errors=errors,
                    )
            default_formats = build_section.get("default_formats")
            if isinstance(default_formats, list) and default_formats:
                for idx, item in enumerate(default_formats):
                    _expect_type(
                        item,
                        str,
                        path=f"thesis.build.default_formats[{idx}]",
                        errors=errors,
                    )
            else:
                errors.append(
                    "thesis.build.default_formats must be a non-empty list of output formats"
                )
        else:
            errors.append("thesis.build must be a mapping")
    else:
        errors.append("thesis must be a mapping")

    rsiena = config.get("rsiena", {})
    if isinstance(rsiena, dict):
        _expect_keys(
            rsiena,
            ["use_cached_results", "cache_strategy", "project_seed", "estimation", "diagnostics", "parallelization"],
            path="rsiena",
            errors=errors,
        )
        if "use_cached_results" in rsiena:
            _expect_type(
                rsiena["use_cached_results"],
                bool,
                path="rsiena.use_cached_results",
                errors=errors,
            )
        if "cache_strategy" in rsiena:
            _expect_type(
                rsiena["cache_strategy"],
                str,
                path="rsiena.cache_strategy",
                errors=errors,
            )
        if "project_seed" in rsiena:
            _expect_type(
                rsiena["project_seed"],
                int,
                path="rsiena.project_seed",
                errors=errors,
            )
        estimation = rsiena.get("estimation")
        if isinstance(estimation, dict):
            _expect_keys(
                estimation,
                ["n3", "n2", "firstg", "max_iterations", "target_phase_tolerance"],
                path="rsiena.estimation",
                errors=errors,
            )
            for key in ["n3", "n2", "max_iterations"]:
                if key in estimation:
                    _expect_type(
                        estimation[key],
                        int,
                        path=f"rsiena.estimation.{key}",
                        errors=errors,
                    )
            for key in ["firstg", "target_phase_tolerance"]:
                if key in estimation:
                    _expect_type(
                        estimation[key],
                        (int, float),
                        path=f"rsiena.estimation.{key}",
                        errors=errors,
                    )
        else:
            errors.append("rsiena.estimation must be a mapping")
        diagnostics = rsiena.get("diagnostics")
        if isinstance(diagnostics, dict):
            _expect_keys(
                diagnostics,
                ["save_convergence_plots", "diagnostics_dir", "report_file"],
                path="rsiena.diagnostics",
                errors=errors,
            )
            if "save_convergence_plots" in diagnostics:
                _expect_type(
                    diagnostics["save_convergence_plots"],
                    bool,
                    path="rsiena.diagnostics.save_convergence_plots",
                    errors=errors,
                )
            for key in ["diagnostics_dir", "report_file"]:
                if key in diagnostics:
                    _expect_type(
                        diagnostics[key],
                        str,
                        path=f"rsiena.diagnostics.{key}",
                        errors=errors,
                    )
        else:
            errors.append("rsiena.diagnostics must be a mapping")
        parallel = rsiena.get("parallelization")
        if isinstance(parallel, dict):
            _expect_keys(
                parallel,
                ["enabled", "cores", "cluster_type"],
                path="rsiena.parallelization",
                errors=errors,
            )
            if "enabled" in parallel:
                _expect_type(
                    parallel["enabled"],
                    bool,
                    path="rsiena.parallelization.enabled",
                    errors=errors,
                )
            if "cores" in parallel:
                _expect_type(
                    parallel["cores"],
                    int,
                    path="rsiena.parallelization.cores",
                    errors=errors,
                )
            if "cluster_type" in parallel:
                _expect_type(
                    parallel["cluster_type"],
                    str,
                    path="rsiena.parallelization.cluster_type",
                    errors=errors,
                )
        else:
            errors.append("rsiena.parallelization must be a mapping")
    else:
        errors.append("rsiena must be a mapping")

    return errors


def parse_arguments(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Validate the thesis reproduction configuration schema."
    )
    parser.add_argument(
        "--config",
        type=Path,
        default=DEFAULT_CONFIG_PATH,
        help="Path to the configuration YAML file (default: reproduced/config/thesis.yml)",
    )
    parser.add_argument(
        "--format",
        choices=["text", "json"],
        default="text",
        help="Output format for validation results (default: text)",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_arguments(argv)
    config_path = args.config
    if not config_path.is_absolute():
        config_path = (Path.cwd() / config_path).resolve()

    if not config_path.exists():
        print(f"Configuration file not found: {config_path}", file=sys.stderr)
        return 2

    try:
        config_data = load_yaml_minimal(config_path)
    except Exception as exc:  # pragma: no cover - defensive messaging
        print(f"Failed to load configuration: {exc}", file=sys.stderr)
        return 2

    errors = validate_config(config_data)

    if args.format == "json":
        output = {
            "config": str(config_path),
            "status": "passed" if not errors else "failed",
            "errors": errors,
        }
        print(json.dumps(output, indent=2))
    else:
        if errors:
            print("Validation failed:")
            for issue in errors:
                print(f" - {issue}")
        else:
            print(f"Configuration valid: {config_path}")

    return 0 if not errors else 1


if __name__ == "__main__":
    sys.exit(main())
