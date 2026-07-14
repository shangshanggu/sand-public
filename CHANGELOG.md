# Changelog

All notable changes to the public SAND research object will be documented here.
The project has not yet issued a public release.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## Unreleased

### Added

- A history-free public-release builder with automated checks for secrets,
  protected-data paths, local machine paths, unsafe symlinks, oversized files,
  and broken relative Markdown/image links.
- A deterministic synthetic proxy-data workflow with a verified Chapters 4–6
  fast path and an optional partial Chapter 7 diagnostic, without distributing
  participant records.
- Structural proxy-output verification that is explicitly separate from
  protected-data empirical benchmark verification.
- A locked R environment, exact RSiena 1.4.7 resolution, fast and full CI
  workflows, citation metadata, security guidance, and an open-science release
  contract.
- A reviewer-facing SAND study book and thesis-to-code crosswalk.
- Visible and machine-readable provenance on generated network HTML, with a
  privacy guard that rejects unlabelled network pages.

### Changed

- Narrowed reproducibility claims to the workflows and checks actually tested.
- Stated explicitly that this is a retrospective reproducibility release, not
  a preregistration.
- Labelled synthetic benchmark differences as warnings rather than evidence of
  empirical reproduction.
- Disabled Chapter 8 by default and placed it outside the verified Chapters 4–6
  public fast path and the partial Chapter 7 diagnostic.
- Separated generated proxy files and protected inputs from the tracked source
  tree.
- Moved optional `networkDynamic`/`ndtv` animation tooling out of the Conda core
  after confirming that `r-networkdynamic` is unavailable from the declared
  channels; the maintained Chapters 4–7 environment now resolves cleanly.

### Fixed

- Corrected proxy AUDIT-C bounds and network-stability wave-pair calculations.
- Added stable synthetic block and flat assignments so the proxy Chapter 7 fit
  exercises, rather than silently fixes, the residence-proximity effects; the
  verifier now rejects degenerate residence covariates.
- Corrected the proxy sex sampling probabilities to match the documented
  `0 = female, 1 = male` encoding.
- Replaced placeholder dependency metadata and validation entry points with
  concrete environment and output checks.
- Aligned README study values and chapter status with the thesis and maintained
  code paths.

### Security

- Removed local credential configuration from the tracked release tree and added
  explicit credential-rotation and protected-data guidance.
