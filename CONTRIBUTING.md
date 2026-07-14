# Contributing

Contributions that improve the accuracy, portability, documentation, or
testability of the SAND reproduction pipeline are welcome.

## Before You Start

1. Read the [pipeline contract](reproduced/REPRODUCIBLE_PIPELINE.md) and
   [data-availability statement](DATA_AVAILABILITY.md).
2. Never add participant data, credentials, contact information, private
   operational notes, or protected outputs to an issue, commit, or pull request.
3. Treat proxy outputs as software demonstrations, not empirical findings.

## Development Workflow

1. Create a focused branch from `main`.
2. Restore the environment with `make env`.
3. Make one reviewable change at a time.
4. Run the smallest relevant check, then the public fast gate:

   ```bash
   make check
   make verify-proxy-quick
   ```

5. If you modify the Chapter 7 model path, also run `make verify-proxy` and
   report its runtime and convergence status. A proxy convergence warning is
   not evidence about the protected-data result.
6. Record the exact commands, platform, exit status, and any warnings in the
   pull-request description.

## Pull Requests

- Explain the scientific or reproducibility problem being addressed.
- Separate generated artefacts from source changes.
- Update tests and documentation when behaviour changes.
- Do not weaken privacy, data-mode, fail-fast, or validation checks to make a
  build pass.
- Require review before merging changes to model specifications, target
  coefficients, privacy rules, or protected-data interfaces.

## Releases

Releases are created from a passing, history-free candidate. A release must
have confirmed rights and authorship, a complete citation record, a changelog,
passing CI, and a versioned archive. See the
[open-science release contract](reproduced/docs/references/open_science_release_contract.md).
