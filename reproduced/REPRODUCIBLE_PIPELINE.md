# Reproducible Pipeline

This document is the build contract for the runnable reproduction workspace under
`reproduced/`.

## Scope (As Of 2026-07-14)

- Maintained code path: **Ch.4–Ch.7**.
- Verified public fast path: **Ch.4–Ch.6** with synthetic proxy inputs.
- **Ch.7** is an optional, long-running manual diagnostic. One proxy run
  completed without convergence; the corrected rerun was intentionally stopped.
  It is not part of the public fast-path guarantee.
- **Ch.8** code exists, but is **disabled by default** (`chapters.chapter8_interventions.enabled: false`).

## Non-Negotiables

- **Config-driven**: scripts read paths and toggles from `reproduced/config/thesis.yml`.
- **Real data is never committed**: approved users stage the protected
  analysis-facing wave bundle in `reproduced/data/raw/`.
- **Proxy data is explicit**: proxy fixtures live under `reproduced/data/proxy/` and are only used when
  `data.mode: proxy`, `SAND_DATA_MODE=proxy`, or `make ... DATA_MODE=proxy`.
- **No silent sample generation**: `make chapter4` fails if raw inputs are missing (it will not auto-generate fixtures).
- **No cached SAOM by default**: `rsiena.use_cached_results: false` forces re-estimation unless you opt in.

## Quick Start (Real Data)

1. Bootstrap the environment:
   ```bash
   make env
   ```
2. Stage the protected analysis inputs (exact filenames):
   - Required: `reproduced/data/raw/list_by_wave.RData`
   - Optional (for hashing/smoke tests): `reproduced/data/raw/participants.csv`,
     `reproduced/data/raw/outcomes.csv` (derive from Wave 1 with `make export-raw-csvs`)
3. Validate the configuration:
   ```bash
   make validate-config
   ```
4. Run the pipeline:
   ```bash
   make all        # Ch.4 -> Ch.7
   # or run chapter-by-chapter
   make chapter4
   make chapter5
   make chapter6
   make chapter7
   ```

## Quick Start (Proxy / Development Mode)

1. Bootstrap the environment:
   ```bash
   make env
   ```
2. Run the verified public fast path. It regenerates proxy inputs, executes
   Chapters 4–6, and checks the expected artefacts:
   ```bash
   make verify-proxy-quick
   ```
3. Optionally run the long Chapter 7 diagnostic:
   ```bash
   make verify-proxy
   ```

## Data Modes

The pipeline supports two data modes:

- `real` (default): use `project.paths.raw_data_dir` (usually `reproduced/data/raw`).
- `proxy`: use `data.proxy_dir` (usually `reproduced/data/proxy`).

### How Mode Is Chosen

1. `SAND_DATA_MODE` environment variable (highest priority)
2. `data.mode` in `reproduced/config/thesis.yml`

When you run via `make`, you can also set `DATA_MODE=proxy`. The Makefile
exports `SAND_DATA_MODE` for the R scripts.

## Outputs, Logs, And Verification

- Chapter outputs: `reproduced/outputs/chapter4/` ... `reproduced/outputs/chapter7/`
- Per-chapter logs/manifests: `reproduced/outputs/chapter*/logs/` and `reproduced/outputs/chapter*/manifests/`
- Pipeline logs: `reproduced/logs/` (created as needed)

Verification:

- `make verify-proxy-quick` regenerates synthetic inputs, runs Chapters 4–6,
  writes checksums, and verifies 16 execution and artefact-structure checks.
  This is the public release gate.
- `make verify-proxy` regenerates synthetic inputs and runs Chapters 4–7. It
  is a long-running manual diagnostic, not a public release gate. Empirical
  benchmark and convergence differences are warnings because proxy data do not
  reproduce the thesis findings.
- `make verify-real` runs Chapters 4–7 on protected data, writes checksums, and requires the recorded chapter benchmarks and Chapter 7 diagnostics to pass.
- `make verify` dispatches to the appropriate target for the selected data mode.

## Random Seeds (Determinism)

Determinism is controlled in configuration:

- Chapter 5 (NAM): `chapters.chapter5_descriptive_norms.nam.set_seed` + `chapters.chapter5_descriptive_norms.nam.seed`
- Chapter 7 (SAOM): `rsiena.project_seed`

Notes:

- Expect small numeric drift across machines for some models due to platform and BLAS differences.
- Proxy fixtures are deterministic by construction, but are not intended to reproduce thesis numbers.

## Chapter Notes

### Chapter 4 (Data Preparation And QA)

Inputs (real mode):

- Required: `reproduced/data/raw/list_by_wave.RData`
- Optional: `reproduced/data/raw/participants.csv`, `reproduced/data/raw/outcomes.csv`
  (derive from Wave 1 with `make export-raw-csvs`)

Key outputs:

- `reproduced/outputs/chapter4/data/norms_longitudinal.rds`
- `reproduced/outputs/chapter4/data/network_arrays.rds`

Degraded/proxy markers:

- `reproduced/data/proxy/.chapter4_synthetic` (small sample fixtures)
- `reproduced/data/proxy/.realistic_proxy_data` (realistic proxy fixtures)

When either marker is present in the active data directory, validation scripts
will treat results as a proxy/degraded run.

### Chapter 5 (Descriptive Norms)

Consumes Chapter 4 outputs and produces:

- `reproduced/outputs/chapter5/tables/nam_summary.csv`
- `reproduced/outputs/chapter5/tables/nam_diagnostics.csv`

### Chapter 6 (Injunctive Norms)

Consumes Chapter 4 manifests and produces approval longitudinal datasets and
tables/figures under `reproduced/outputs/chapter6/`.

### Chapter 7 (SAOM)

Thesis alignment:

- Baseline covariates are taken from the Wave 1 snapshot.
- SAOM network/behaviour observations are aligned to the thesis timepoints:
  Waves **2, 4, 5, 6** (Oct-22, Dec-22, Mar-23, Oct-23).

This alignment is implemented in:

- `reproduced/analyses/chapter7_saom/scripts/00_build_network_arrays_base.R`

## Common Failure Modes

- `make chapter4` fails with missing inputs:
  - Fix: stage real exports into `reproduced/data/raw/`, or run `make proxy-data` and re-run with
    `SAND_DATA_MODE=proxy` (or `DATA_MODE=proxy`).

- SAOM convergence or long runtimes:
  - Inspect the Chapter 7 run log and the current public verification record in
    `reproduced/docs/status/2026-07-14_public_release_verification.md`.

## Pointers

- Documentation index: `reproduced/docs/README.md`
- Configuration guide: `reproduced/docs/references/configuration_guide.md`
- Onboarding pointer map: `reproduced/docs/references/onboarding_pointer_map.md`
