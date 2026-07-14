# Open Science Release Contract

Back to the [documentation hub](../README.md).

## Purpose

This contract defines what “publishable and open science” means for SAND. Public code alone is insufficient. The release must make its scientific scope, provenance, data restrictions, software environment, validation coverage, and limitations inspectable.

## Release Object

The clean public repository includes:

- maintained Chapters 4–7 code, with a verified Chapters 4–6 public fast path
  and Chapter 7 explicitly marked as a partial manual diagnostic;
- explicit real and proxy data modes;
- a synthetic proxy-data generator and schema documentation;
- environment and dependency locks;
- thesis-to-code and output crosswalks;
- mode-appropriate tests and validation reports;
- citation, contribution, security, and data-availability documents;
- a licence confirmed by the rights holder;
- a release manifest, with a versioned archive planned after publication.

It excludes participant data, credentials, private Git history, internal project-management files, agent instructions, personal notes, and unvalidated Chapter 8 claims.

This is a retrospective reproducibility release, not a preregistration. The
repository must not imply that the study or analyses were registered before
data collection or inspection.

## Evidence Required

| Requirement | Evidence |
|---|---|
| No secrets or participant data | Automated tracked-tree scan plus manual release review |
| Honest proxy boundary | Data-availability statement, proxy metadata, and visible synthetic labels |
| Reproducible environment | `environment.yml`, `renv.lock`, Docker smoke test, and recorded platform details |
| Executable public workflow | Fresh clone, proxy generation, chapter builds, and captured exit status |
| Scientific validation | Real-data benchmark checks kept distinct from proxy structural/smoke checks |
| Provenance | Thesis crosswalk, input/output manifests, seeds, checksums, and code version |
| Reusable code | Confirmed software licence and third-party rights review |
| Citable release | `CITATION.cff`, version tag, changelog, and archived release DOI when available |
| Transparent limitations | README and study book state missingness, model, generalisability, and release limits |

## Current Release Gates

- [x] Exclude credentials and private development history from the clean public
      repository; the exact release scan passes.
- [x] Remove local secret configuration from the tracked tree.
- [x] Confirm the code and documentation licence with the rights holder (MIT
      for code; CC BY 4.0 for documentation and diagrams).
- [x] Confirm the ethics approval date, institutional administration, and
      reference against the approval letter.
- [x] Confirm authorship and licence authority with the owner.
- [x] Exclude participant data and study instruments and offer no data-access
      route. Consent and data-controller confirmation remain prerequisites for
      any future data or instrument release, not for this software-only release.
- [x] Replace ambiguous `make verify` behaviour with explicit proxy and real-data checks.
- [x] Make CI generate proxy inputs explicitly.
- [x] Replace the placeholder dependency lock and add renv activation.
- [x] Run the public fast path from a clean checkout and record the results
      (Chapters 4–6 passed). Record Chapter 7 separately as a long-running,
      non-converged partial path rather than a public-release guarantee.
- [x] Reconcile README, configuration, chapter status, and scenario counts for the Chapters 4–7 release scope.
- [x] Embed visible and machine-readable provenance in generated network HTML and
      make the privacy guard reject unlabelled network pages.
- [x] Audit the candidate source tree for secrets, protected-data paths, private notes, local paths, external symlinks, and oversized files.
- [x] Build a curated public tree without development history.
- [x] Test relative links, citation metadata, and the locked installation and
      Chapters 4–6 commands in the exact history-free public tree.
- [x] Run the hosted fast CI workflow after publication; the first public run
      passed environment restore, tests, and Chapters 4–6 proxy verification
      ([run 29366951200](https://github.com/shangshanggu/sand-public/actions/runs/29366951200)).
- [ ] Build and test the optional Docker image when a daemon is available.
- [x] Test the public repository and website while signed out; both returned
      successfully on 14 July 2026, and the hosted page matched
      `docs/index.html` byte for byte.
- [ ] Create and test the versioned release archive after publication.
- [ ] Tag and archive after the applicable publication gates above pass.

## Interpretation Rule

A successful proxy run proves that the public software path executes on synthetic inputs. It does not prove that proxy estimates match thesis results. A successful protected real-data validation proves agreement with recorded empirical benchmarks, subject to the stated tolerances. Release documentation must keep these claims separate.
