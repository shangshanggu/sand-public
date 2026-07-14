# README

## Chapter 5: Descriptive Norm Misperception

This pipeline estimates network autocorrelation models (NAMs) linking personal
AUDIT-C scores with perceived typical-resident drinking and perceived peer
drinking at three thesis-aligned time periods.

It consumes the Chapter 4 longitudinal and network products, constructs the NAM
analysis matrices, estimates the configured models, generates coefficient plots,
and compares outputs with recorded thesis targets.

```bash
make DATA_MODE=proxy chapter5
```

Outputs are written under `reproduced/outputs/chapter5/`: analysis tables,
diagnostics, figures, run logs, checksums, and comparison manifests. In proxy
mode, differences from protected-data thesis coefficients are expected warnings.
In real mode, the configured tolerances are enforced as empirical validation
checks.

See the [pipeline contract](../../REPRODUCIBLE_PIPELINE.md) and
[Chapter 5 output schema](../../docs/references/chapter5_nam_summary_schema.md).
