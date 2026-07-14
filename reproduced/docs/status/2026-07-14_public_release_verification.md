# 2026-07-14 Public Release Verification

Back to the [documentation hub](../README.md) and the
[open-science release contract](../references/open_science_release_contract.md).

## Purpose

This note records evidence gathered from history-free release candidates. It is
not a publication approval. Proxy results demonstrate executable software and
artifact structure; they do not reproduce or validate the protected empirical
findings.

## Platform

| Item | Value |
|---|---|
| Date | 14 July 2026 |
| Operating system | macOS 15.6 (24G84) |
| Architecture | arm64 |
| Processor | Apple M4, 10 physical cores |
| Memory | 16 GiB |
| R | 4.3.2 |
| RSiena | 1.4.7 |

## Canonical Public-Tree Results

Source: the fresh `sand-public` tree created by
`scripts/00_setup/build_public_release.py` without the private development
history. The public repository begins from this curated snapshot.

| Check | Result | Interpretation |
|---|---|---|
| Candidate disclosure/secret scan | Pass | No protected paths, known secret forms, local absolute paths, unsafe symlinks, or oversized files detected by the release builder |
| Conda environment solve | Pass | `environment.yml` resolves from the declared channels on macOS arm64 |
| Locked R library restore | Pass | `renv.lock` restored into a clean project library |
| Python unit suite | Pass | Thirteen tests, including release-builder integration, curated-tree exclusions, disclosure/link audits, and verifier behaviour |
| R portfolio/integration suite | Pass | Data dictionary, document limits, network explorer, integration, privacy, manifest, and dashboard tests pass |
| Fast proxy pipeline | Pass with expected warnings | The exact `sand-public` tree restored `renv.lock`, executed Chapters 4–6, and passed 16 structural checks with zero failures and two expected proxy-versus-protected benchmark warnings |
| Portfolio generation/privacy | Pass | Dictionary, manifest, dashboard, and five labelled network pages generated; both privacy scans pass |
| First full Chapter 7 diagnostic | Completed with warnings | The old proxy fixture produced a 105 MB fit and all expected artefacts in 8,480 seconds, but did not converge and did not exercise residence dyads |
| Corrected full Chapter 7 proxy path | Intentionally stopped | The corrected path ran for about 52 minutes before being stopped to prioritise the time-bounded public release; no corrected fit is claimed |
| Docker image | Not tested | Docker CLI is installed locally, but the daemon is unavailable |
| Hosted fast CI | Pass | The first public R-CI run completed successfully on GitHub-hosted Ubuntu and passed environment restore, tests, and Chapters 4–6 proxy verification ([run 29366951200](https://github.com/shangshanggu/sand-public/actions/runs/29366951200)) |
| Signed-out public access | Pass | The repository and `https://sand.shangshanggu.com` both returned successfully without authentication on 14 July 2026; the hosted page matched `docs/index.html` byte for byte |

## Residence-Covariate Correction Found During Chapter 7

Inspection of the first long proxy fit showed zero target statistics for the
flatmate and blockmate effects. The main proxy generator supplied only a generic
residence cluster, while the Chapter 7 builder correctly expected separate
`number_block` and `number_flat` fields. RSiena therefore fixed both effects
instead of exercising them.

The proxy generator now produces stable, balanced synthetic block and flat
assignments across all six waves. A regression test requires both dyadic
covariates to be non-zero, and the output verifier rejects a Chapter 7 run that
records degenerate residence covariates. The proxy sex-sampling probability was
also corrected to match the documented `0 = female, 1 = male` encoding.

The first long fit remains useful as a pipeline diagnostic. The corrected rerun
was intentionally stopped after about 52 minutes to prioritise the public
Chapters 4–6 release. Chapter 7 is therefore documented as a partial,
long-running path, not part of the public fast-path guarantee.

Chapter 4 contained a second residence-related defect: its intermediate network
builder constructed flatmate and blockmate matrices from actor row order rather
than the declared residence fields. That substitution has been removed. Both
real and proxy modes now require `number_block` and `number_flat`, fail on
missing or degenerate residence data, and record the dyadic non-zero counts in
the Chapter 4 manifest. The corrected proxy fixture records 666 directed
flatmate dyads and 5,166 directed blockmate dyads; the Chapter 4 verifier passes
all eight structural checks.

Obsolete placeholder generators were also removed from the maintained Chapter
7 source. Defensive checks still reject historical placeholder fit objects,
but the public code no longer contains a path that can create them.

### First diagnostic result

The initial history-free candidate completed the full Chapters 4–7 command in
8,480.1 seconds (2 h 21 min 20 s). It produced a 105,354,034-byte SAOM fit,
coefficient tables, validation outputs, a figure, logs, manifests, and network
HTML. The proxy verifier recorded 21 checks, zero structural failures, and three
warnings under the then-current rules.

The RSiena diagnostic status was `diagnostic_failure`: `tconv.max = 8.9481`
against a `0.1` tolerance, with the behaviour linear-shape target also outside
tolerance. Both residence effects had zero target statistics and were fixed.
This proves that one long software path completed and emitted inspectable
artefacts. It does not establish proxy-model convergence or empirical validity.

## Network Provenance

Every generated proxy network page now includes:

- the visible heading `SYNTHETIC PROXY DATA`;
- `data-sand-data-mode="proxy"` in the HTML;
- synthetic actor and synthetic nomination wording;
- an explicit statement that no participant records or real network ties are
  shown.

The portfolio privacy guard rejects a network page missing the visible label or
machine-readable proxy marker. Protected-data pages receive a red
`PROTECTED REAL DATA — DO NOT PUBLISH` banner.

## Environment Correction Found During Verification

The first current-candidate bootstrap failed because Conda no longer provides
`r-networkdynamic` from the declared channels. That package is used only by the
optional microstep-animation script, which is outside the maintained Chapters
4–7 dependency scan. The Conda pin was removed and the optional
`networkDynamic`/`ndtv` CRAN installation was documented separately. A fresh
Conda solve then passed, followed by a successful locked R-library restore.

## Release Gates Still Open

- build and test the optional Docker image when a daemon is available;
- tag and archive a versioned release after those publication checks.
