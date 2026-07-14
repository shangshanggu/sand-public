# README

## Chapter 7: Social Selection and Influence

Chapter 7 uses RSiena stochastic actor-oriented models to separate friendship
selection from peer influence in drinking behaviour. The maintained model uses
survey Waves 2, 4, 5, and 6, baseline actor covariates, and flatmate/blockmate
dyadic covariates.

The model specification is declared in
`reproduced/config/scenarios/saom_models.yml`; execution settings, seeds,
diagnostic tolerances, and target coefficients are in
`reproduced/config/thesis.yml`.

```bash
make DATA_MODE=proxy chapter7
python3 reproduced/scripts/00_setup/verify_outputs.py --mode proxy --chapters 7
```

The pipeline builds thesis-aligned network and behaviour arrays, prepares RSiena
inputs, estimates or explicitly resumes the configured fit, generates tables and
figures, and writes machine-readable diagnostics. Missing data, missing RSiena,
placeholder inputs, and degenerate residence covariates fail loudly. Historical
placeholder fit objects may be detected and rejected but are never generated.

Outputs are written under `reproduced/outputs/chapter7/`, including input and
output manifests, fit cache, coefficient tables, figures, validation payloads,
and run logs. A proxy fit that fails to converge is a software-path warning, not
an empirical result. Protected-data validation requires the declared convergence
and coefficient checks to pass.

See the [pipeline contract](../../REPRODUCIBLE_PIPELINE.md),
[thesis crosswalk](../../docs/references/thesis_scope_and_crosswalk.md), and
[verification evidence](../../docs/status/2026-07-14_public_release_verification.md).
