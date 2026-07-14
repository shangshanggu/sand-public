# Onboarding Pointer Map

This guide orients new contributors to the SAND thesis reproduction repository. It explains where things live, how to run the pipelines with proxy or approved protected data, and how to navigate the public documentation.

## 1) Repository at a Glance
- **Canonical root:** `reproduced/` holds everything you should edit or run: configs, data, scripts, outputs, docs, logs.
- **Key folders (under `reproduced/`):**
  - `config/` – `thesis.yml` plus the Chapter 7 model specification.
  - `data/raw/` – approved users stage the protected analysis bundle here
    (`list_by_wave.RData` required; `participants.csv`/`outcomes.csv` optional
    or derived); proxy fixtures live in `data/proxy/`.
  - `analyses/` – maintained Chapter 4–7 scripts plus disabled experimental Chapter 8 work.
  - `outputs/` – chapter results, figures, tables, logs.
  - `logs/` – pipeline history and validation logs (e.g., `run_all_history.json`, `validate-config/`, `chapter*/`).
  - `scripts/` – setup helpers, utilities, portfolio builders, and visualisations.
  - `Makefile` (root) and `reproduced/Makefile` – entrypoints for env + chapters.

## 2) Quick Entry Points
- Environment: `make env` (Conda + renv), `make validate-config`.
- Run everything (Ch.4–7): `make all` (preferred) or `Rscript reproduced/run_all.R` (pure R orchestrator).
- Per chapter: `make chapter4`, `make chapter5`, `make chapter6`, `make chapter7`.
- Chapter 8 is outside the public release contract and disabled by default.
- Derive optional raw CSVs: `make export-raw-csvs`.
- Logs/results: see `reproduced/outputs/chapter*/` and `reproduced/logs/`.

## 3) Prereqs and Environment (Conda ↔ R ↔ libraries)
- What’s pinned: `reproduced/environment.yml` defines the Conda base environment with R 4.3.2 and system dependencies; `reproduced/renv.lock` records the exact maintained R dependency graph. The project-local RSiena installer enforces version 1.4.7.
- How it’s used: `make env` first applies `environment.yml`, then runs `renv::restore()` for the project library. All `make` targets run via `conda run -n r_stable` and the project `.Rprofile` activates the locked R library.
- If you change packages: update both `environment.yml` (Conda layer) and `renv.lock` (R layer), then document the change (see `environment_reproducibility_guide.md` for the workflow).
- Prereqs quick steps (copy/paste):
  - Miniforge (Conda) install:
    - macOS Intel: `curl -L https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-MacOSX-x86_64.sh -o miniforge.sh && bash miniforge.sh -b -p "$HOME/miniforge3"`
    - macOS Apple Silicon: `curl -L https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-MacOSX-arm64.sh -o miniforge.sh && bash miniforge.sh -b -p "$HOME/miniforge3"`
    - Linux x86_64: `curl -L https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-x86_64.sh -o miniforge.sh && bash miniforge.sh -b -p "$HOME/miniforge3"`
    - Then: `source "$HOME/miniforge3/etc/profile.d/conda.sh"` and verify `conda --version`.
  - If `make env` says the env is missing: run this once from the repo root to create it explicitly, then rerun `make env`:
    ```bash
    conda env create -n r_stable -f reproduced/environment.yml
    ```
  - Host R vs Conda R: a host R installation is not required. All `make` targets use Conda R 4.3.2. Quick sanity check:
    ```bash
    conda run -n r_stable Rscript --version   # expect 4.3.2
    make validate-config                      # should pass using Conda R
    ```

## 4) Documentation Map
- Project overview & layout: `reproduced/README.md`.
- Build contract & commands: `reproduced/REPRODUCIBLE_PIPELINE.md`.
- Doc index: `reproduced/docs/README.md`.
- Thesis-to-code map: `reproduced/docs/references/thesis_scope_and_crosswalk.md`.
- Release evidence standard: `reproduced/docs/references/open_science_release_contract.md`.
- Current verification evidence: `reproduced/docs/status/2026-07-14_public_release_verification.md`.

## 5) Proxy Data Workflow (no real REDCap data needed)
- What it does: Generates proxy `list_by_wave.RData`, `participants.csv`, and `outcomes.csv` under `reproduced/data/proxy/` so Chapters 4–7 can execute as a synthetic structural demonstration.
- Boundary: mode is recorded explicitly in configuration, environment variables, manifests, and generated outputs; proxy estimates are never empirical results.
- How to trigger:
  1. Run `make proxy-data` to generate proxy inputs under `reproduced/data/proxy/`.
  2. Set `data.mode: proxy` in `reproduced/config/thesis.yml` (or run `SAND_DATA_MODE=proxy make chapter4`).
  3. Continue with `make chapter5`, `make chapter6`, `make chapter7` (or stay with `make all`).
- What to expect: Outputs populate under `reproduced/outputs/chapter*/` from synthetic inputs; logs mark the data boundary and any degraded empirical diagnostics. These are demonstration outputs, not placeholder thesis results.
- Verification: `make verify-proxy` checks execution and artifact structure. It records thesis-benchmark or convergence differences as warnings because synthetic inputs are not empirical evidence.

## 6) Transition to Real Data
- Stage the protected analysis bundle in `reproduced/data/raw/` with the exact
  name `list_by_wave.RData`. This R object is assembled downstream of the
  controlled REDCap CSV exports; it is not a native REDCap export format.
- Optional: Derive `participants.csv` and `outcomes.csv` from Wave 1 with `make export-raw-csvs` (useful for hashing and smoke tests).
- Set mode: Ensure `data.mode: real` (or clear `SAND_DATA_MODE`) so the pipeline reads from `reproduced/data/raw/`.
- Validate & run: `make validate-config`, then `make chapter4` (or `make all`). Run `make verify-real` to record hashes and enforce chapter benchmarks and Chapter 7 diagnostics.
- Outputs: Real-data runs will overwrite previous proxy outputs in `reproduced/outputs/chapter*/`.

## 7) “I want to do X—where do I start?”
- Just explore structure → skim Sections 1 & 3; browse `reproduced/README.md`.
- Run a proxy end-to-end pass → Section 5; commands: `make proxy-data`, then `SAND_DATA_MODE=proxy make all`.
- Inspect experimental Chapter 8 work → treat it as outside the verified public pipeline.
- Run with real data → Section 5; then `make validate-config`, `make all`.
- Inspect logs/results → `reproduced/logs/`, `reproduced/outputs/chapter*/logs/`.

## 8) Tips & Pitfalls
- Always run through Conda: `make` targets call `conda run -n r_stable` for you.
- Missing Rscript/Conda → install prerequisites before `make env`.
- Always set or confirm the data mode explicitly before running; real and proxy outputs share output directories.
- If manifests or logs seem stale, rerun `make validate-config` and the affected chapter target.
