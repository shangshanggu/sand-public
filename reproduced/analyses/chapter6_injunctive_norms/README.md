# README

## Chapter 6: Injunctive Norm Misperception

This pipeline prepares longitudinal perceived-approval measures for three risky
drinking scenarios, summarises their change over time, estimates the configured
NAMs, and checks the resulting coefficients against recorded thesis targets.

```bash
make DATA_MODE=proxy chapter6
```

Outputs are written under `reproduced/outputs/chapter6/`: longitudinal data,
summary and coefficient tables, figures, manifests, selected public exports,
logs, and checksums. Proxy runs exercise structure and software; protected-data
runs enforce the empirical benchmark contract.

See the [pipeline contract](../../REPRODUCIBLE_PIPELINE.md) and
[thesis crosswalk](../../docs/references/thesis_scope_and_crosswalk.md).
