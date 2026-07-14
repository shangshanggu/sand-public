# Chapter5 Nam Summary Schema

Schema for the Chapter 5 NAM summary bundle (config-driven combo). Model inputs and summaries are generated under `reproduced/outputs/chapter5/` by `01_prepare_chapter5_data.R` and `02_estimate_nam_models.R`, sourcing exclusively from the Chapter 4 manifest. The legacy folder `reproduced/data/chapter5_nam_summary/` is no longer required for the pipeline. See `reproduced/docs/README.md` for how this reference fits into the docs layout.

## Summary File — `nam_summary.csv` (generated at `reproduced/outputs/chapter5/tables/`)

Required columns:
- `time_period` (string) — e.g., `Time 1`, `Time 2`, `Time 3`.
- `term` (string) — `global_misperception`, `peer_misperception`.
- `estimate` (numeric) — coefficient estimate from `audit_score ~ misperception_audit_c_global + misperception_audit_c_peer`.
- `std_error` (numeric) — standard error.
- `t_value` (numeric) — t statistic.
- `p_value` (numeric) — p value.

## Model Data Files — `model_data_<model_index>.csv`

Generated to `reproduced/outputs/chapter5/data/model_data/` from Chapter 4 prepared datasets (LOCF + mean). Required columns:
- `participant_id` (string/integer)
- `misperception_audit_c_global` (numeric)
- `misperception_audit_c_peer` (numeric)
- `audit_score` (numeric)

Other columns are tolerated but unused. Filenames align with `model_index` from the summary.
