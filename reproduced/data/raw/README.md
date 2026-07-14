# README

This directory stages the **protected analysis inputs** required to run the
pipeline in real-data mode. Despite the directory name, `list_by_wave.RData` is
an analysis-facing object assembled downstream of controlled REDCap CSV
exports, not a native REDCap export.

## What Goes Here

Stage these files with exact names:
- `list_by_wave.RData` (required for analysis)
- `participants.csv` (optional; can be derived from Wave 1)
- `outcomes.csv` (optional; can be derived from Wave 1)

## If You Only Have list_by_wave.RData

You can derive the optional CSVs from the Wave 1 snapshot in `list_by_wave.RData`:

```bash
make export-raw-csvs
# or
Rscript reproduced/scripts/00_setup/export_raw_csvs_from_list_by_wave.R
```

These CSVs are convenience outputs for hashing, smoke tests, and notebooks. They
are not a replacement for institutionally exported raw files if you have them.
The helper refuses to overwrite existing CSVs unless you pass `--overwrite`.

## Important

- This directory is intentionally ignored by Git (`reproduced/data/raw/**`) so
  sensitive data is not committed.
- The pipeline default is `data.mode: real` (see `reproduced/config/thesis.yml`).
- `list_by_wave.RData` must contain a list named `list_by_wave`, with one
  analysis-facing data frame per wave. The maintained Chapter 4 workflow
  requires stable `number_block` and `number_flat` fields to construct
  residence dyadic covariates; it stops instead of inferring these fields from
  row order.
- Stage only coded exports with direct identifiers excluded. Names, email
  addresses, contact lists, identity keys, and REDCap credentials do not belong
  in this directory or repository.
