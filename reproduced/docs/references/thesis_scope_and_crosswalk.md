# Thesis Scope And Crosswalk

## Purpose

This document re-states what the repository is aiming to reproduce from the PhD
thesis, and maps thesis chapters to the runnable pipelines under `reproduced/`.

## What This Repo Is Trying To Reproduce

In scope (current focus):
- Rebuild **Chapter 4–7** analysis outputs from the staged protected analysis
  bundle using the
  scripted pipeline (`make chapter4` ... `make chapter7`).
- Regenerate the thesis-facing tables/figures and model summaries produced by
  those chapters (not the narrative text).

Out of scope (for now):
- Full Chapter 8 intervention simulation runs are present as scaffolding but are
  disabled by default (`chapters.chapter8_interventions.enabled: false`).
- End-to-end thesis compilation is not required for validating the analytical
  pipeline (packaging hooks exist under `thesis.*` in config if needed later).

## Canonical Data Timeline (Waves)

The study runs across six survey waves (baseline + follow-ups). The repository
treats `list_by_wave.RData` as its canonical analysis input (Wave 1 baseline
snapshot plus Waves 2–6). This object was assembled downstream of controlled
REDCap CSV exports; it is not a native REDCap export format.

Key thesis-aligned timepoints used downstream:
- Chapter 7 SAOM runs are aligned to Waves **2, 4, 5, 6** (Oct-22, Dec-22, Mar-23, Oct-23).

## Chapter Crosswalk

### Chapter 4: Data Collection, Coverage, And QA

Thesis outputs:
- Participation/retention metrics across waves.
- Network nomination volume and stability summaries (e.g., Jaccard indices).
- Derived longitudinal norms measures used by later chapters.

Repo pipeline:
- Scripts: `reproduced/analyses/chapter4_data_collection/scripts/`
- Primary inputs (real mode): files in `reproduced/data/raw/` (see `reproduced/REPRODUCIBLE_PIPELINE.md`).
- Primary outputs: `reproduced/outputs/chapter4/` (datasets, manifests, QA logs).

### Chapter 5: Descriptive Norms (NAM)

Thesis outputs:
- NAM coefficient tables and diagnostics that quantify descriptive norm effects.

Repo pipeline:
- Scripts: `reproduced/analyses/chapter5_descriptive_norms/scripts/`
- Inputs: Chapter 4 longitudinal norms outputs.
- Outputs: `reproduced/outputs/chapter5/` (including `tables/nam_summary.csv`).
- Numerical reference targets are encoded in `reproduced/config/thesis.yml` and checked by the Chapter 5 validation scripts.

### Chapter 6: Injunctive Norms

Thesis outputs:
- Approval trajectories, contrasts, and supporting tables/figures.

Repo pipeline:
- Scripts: `reproduced/analyses/chapter6_injunctive_norms/scripts/`
- Inputs: Chapter 4 manifests and longitudinal datasets.
- Outputs: `reproduced/outputs/chapter6/`.

### Chapter 7: Social Selection And Influence (SAOM)

Thesis outputs:
- Summary metrics across Waves 2, 4, 5, 6.
- SAOM estimation results table (selection + influence effects).
- Goodness-of-fit diagnostics plots.

Repo pipeline:
- Scripts: `reproduced/analyses/chapter7_saom/scripts/`
- Inputs: the canonical `list_by_wave.RData` bundle plus the Chapter 7 model specification in `reproduced/config/scenarios/saom_models.yml`.
- Outputs: `reproduced/outputs/chapter7/` (inputs, logs, fitted objects, tables/figures).
- Thesis wave alignment implementation: `reproduced/analyses/chapter7_saom/scripts/00_build_network_arrays_base.R`.

Key numeric anchors used for validation:
- Chapter 5 NAM (global misperception): see the targets in `reproduced/config/thesis.yml` and the generated `reproduced/outputs/chapter5/tables/nam_summary.csv`.
- Chapter 7 SAOM (peer influence / average similarity): configured under `chapters.chapter7_saom.target_coefficients` in `reproduced/config/thesis.yml` (expected magnitude around 1.884 for thesis-aligned runs).

## Where To Start When You Feel Lost

1. `reproduced/REPRODUCIBLE_PIPELINE.md` (how to run, real vs proxy mode).
2. `reproduced/config/thesis.yml` (single source of truth for paths/toggles).
3. `reproduced/docs/references/onboarding_pointer_map.md` (staging and validation checklist).
4. `reproduced/docs/status/2026-07-14_public_release_verification.md` (what has actually been tested).
