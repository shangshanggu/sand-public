# Binary Storage Guidance

## Changelog
- 2025-09-23 — Established the large-binary handling policy.
- 2026-07-14 — Reframed the policy for the public, history-free release.

## Context
The project must record the provenance of protected inputs and large generated
artefacts without putting participant data or bulky, frequently changing
outputs in Git. SHA-256 manifests let approved users verify locally staged
files without distributing the files themselves.

## Decision
1. Treat the Git repository as metadata and automation source only. Raw and
   derived binary artefacts live outside Git unless they are under 5 MB and are
   essential for tests.
2. Keep protected inputs in institutionally approved storage. The public
   repository supplies staging instructions and schemas, never access URLs or
   credentials.
3. Put citable public artefacts in versioned release archives only after they
   pass the disclosure and rights review.
4. Record SHA-256 hashes for every dataset with
   `scripts/00_setup/generate_data_hashes.py` and publish the manifest to
   `reproduced/logs/data_hashes.json`. Review any manifest before publication;
   paths and filenames can disclose sensitive operational details.
5. Describe omitted artefacts in Markdown manifests with their purpose,
   checksum, creation command, and access boundary.

## Options Considered
- **Commit binaries to Git:** Simple, but unsuitable for protected participant
  data and noisy for generated outputs.
- **Git LFS:** Offers transparent checkout but requires additional tooling and
  is unavailable in some execution sandboxes. Chosen as a secondary option only
  if object storage becomes unavailable.
- **Approved storage with reviewed manifests (selected):** Keeps Git lean and
  preserves verifiable provenance without publishing protected locations.

## Follow-Up Actions
- Add an institutional data-access procedure if the data controller authorises
  one.
- Archive only disclosure-reviewed public outputs with a tagged release.
- Keep checksum generation and verification in the maintained Make workflow.
