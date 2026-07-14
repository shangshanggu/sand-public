# Environment Reproducibility Guide

Back to the [documentation hub](../README.md).

## Supported Environment

The maintained Chapters 4–7 pipeline uses three coordinated layers:

1. `environment.yml` creates the `r_stable` Conda environment with R 4.3.2,
   Python 3.11, compilers, and system-facing packages.
2. `renv.lock` records the exact R dependency graph used by the maintained
   source tree. `renv/activate.R` selects the project library.
3. `make rsiena` verifies RSiena 1.4.7 and, when necessary, installs that exact
   archived source release into the ignored local library `.Rlib/`.

The local `.Rlib/` takes precedence so Chapter 7 cannot silently run with a
different globally installed RSiena version.

## Bootstrap

From the repository root:

```bash
make env
make validate-config
```

`make env` updates or creates the Conda environment and restores the project R
library from `renv.lock`. To run the layers separately:

```bash
make env-conda
make env-renv
make rsiena
```

All analysis targets use `conda run -n r_stable`; the host R installation is
not part of the run contract.

## Dependency Scope

`.renvignore` excludes generated/protected data, documentation examples,
disabled Chapter 8 code, and the optional microstep-animation renderer from
the core dependency scan. Those exclusions are deliberate: the public release
does not claim those paths as part of the verified Chapters 4–6 fast path or
the partial Chapter 7 diagnostic.

The animation script additionally needs `networkDynamic`, `ndtv`, and their
system dependencies. They are installed separately from CRAN because
`r-networkdynamic` is not available from the declared Conda channels. This path
is optional and must not be used as evidence that the core pipeline passed.

## Updating Dependencies

1. Change `environment.yml` when a Conda or system dependency changes.
2. Recreate or update `r_stable`.
3. From `reproduced/`, snapshot the maintained R source tree:

   ```bash
   conda run -n r_stable Rscript -e 'renv::snapshot(prompt = FALSE)'
   ```

4. Confirm `renv.lock` contains no placeholder hashes and records R 4.3.2 and
   RSiena 1.4.7.
5. Run `make check`, `make verify-proxy-quick`, and the relevant full workflow.
6. Record the commands, platform, and outcome in a dated status note.

## Docker

The root `Dockerfile` restores the project at `reproduced/` from the same
`renv.lock`, then runs the environment smoke test. Build it with:

```bash
make docker-build
```

A successful image build verifies dependency restoration and package loading;
it does not replace the chapter-level proxy or protected-data checks.

## Release Checks

- `renv.lock` parses as JSON and contains real package hashes.
- a fresh project library can restore every locked package;
- `make rsiena` resolves exactly version 1.4.7;
- `make check` passes;
- `make verify-proxy-quick` passes from a clean release candidate (confirmed on
  macOS arm64, 14 July 2026);
- Docker and the full Chapter 7 workflow are recorded separately when run.
