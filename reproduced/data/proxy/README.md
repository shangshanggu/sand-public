# README

This directory holds **synthetic proxy inputs** for public demonstrations and
pipeline checks. The generator uses study-level design parameters, but it does
not transform or anonymise participant records. Do not use proxy results for
substantive inference.

## How To Generate

From the repo root:
```bash
make proxy-data
```

## How To Use

Proxy mode is always explicit:
```bash
SAND_DATA_MODE=proxy make all
# or
make all DATA_MODE=proxy
```

## Markers

Proxy runs are tagged via marker files in this directory:
- `.realistic_proxy_data` (realistic proxy generator output)
- `.chapter4_synthetic` (small sample fixture output)

## Schema Boundary

The generated `list_by_wave.RData` mirrors the analysis-facing wave-list
structure. In particular, `number_block` and `number_flat` provide stable
synthetic residence membership for the Chapter 4 flatmate and blockmate dyadic
covariates. `list_by_wave_schema.csv` records the generated columns and storage
modes. Generated row-level proxy files are gitignored and rebuilt with
`make proxy-data`.
