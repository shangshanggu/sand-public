# README

## Chapter 4: Longitudinal Social-Network Data Collection and Preparation

Chapter 4 documents the SAND study's REDCap-based method for collecting repeated
behaviour measures and sociocentric peer nominations, then prepares the staged
export for Chapters 5–7. The public repository reproduces the export-to-analysis
boundary; it does not distribute the live REDCap projects, contact data, identity
key, or participant records.

For the study design, REDCap/MySQL stack, identity-separation workflow,
recruitment and retention operations, incentive automation, and field-level
reference, open the [Study & REDCap Reference](../../../docs/index.html).

## Input Contract

The canonical input is `list_by_wave.RData`, an analysis-facing object assembled
from the six controlled CSV exports. Approved users place it in
`reproduced/data/raw/`. Public users create a schema-compatible synthetic bundle
with:

```bash
make proxy-data
```

Proxy inputs are synthetic and visibly marked. They do not transform or
anonymise participant rows.

## Methodological Decisions Encoded Here

- Direct identifiers and the identity key remain outside the analysis boundary.
- Peer nominations are directed and are limited to ten slots per wave.
- Actor order is keyed by `redcap_survey_identifier`; dyadic covariates are built
  from declared `number_block` and `number_flat` fields, never row position.
- Missing residence fields and degenerate residence matrices stop the build.
- Peer and global norm measures, misperception variables, and configured
  imputation variants are generated from the staged wave bundle.
- Real and proxy modes share the same scripts but have distinct validation
  interpretations.

## Scripts and Outputs

| Script | Role | Main outputs |
|---|---|---|
| `01_data_preparation_norms.R` | Load and reshape waves; derive norms, actor covariates, and network/dyadic arrays | `prepared_data_sets.RData`, `network_arrays.rds`, prepared-data manifest |
| `02_qa_checks_norms.R` | Check participation coverage, retention, and data-mode provenance | QA report and assertions |
| `03_export_norms_longitudinal.R` | Assemble the longitudinal norms dataset | `norms_longitudinal.rds`, summary table |
| `04_export_chapter5_bridge.R` | Materialise the Chapter 5 analysis bridge | NAM input table and metadata |

Generated files live under `reproduced/outputs/chapter4/`; they are not tracked
as source.

## Run and Verify

```bash
make DATA_MODE=proxy chapter4
python3 reproduced/scripts/00_setup/verify_outputs.py --mode proxy --chapters 4
```

For the complete public fast path, use `make verify-proxy-quick`. See the
[pipeline contract](../../REPRODUCIBLE_PIPELINE.md) and
[data-availability statement](../../../DATA_AVAILABILITY.md) for the full
boundary and interpretation rules.
