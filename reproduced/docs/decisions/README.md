# README

This directory records consequential technical and governance choices so future
contributors can understand why the pipeline works as it does.

## How to Use This Directory
1. Start every record with the structure from `decision_record_template.md` so metadata, options, and follow-up actions remain consistent.
2. Name files `YYYY-MM-DD-short-title.md` to maintain chronological ordering (e.g., `2025-09-21-environment-governance.md`).
3. Reference related issues, pull requests, evidence, and validation commands.
4. Update `../README.md` and any affected contributor guides when the decision changes day-to-day workflows.

Use `CHANGELOG.md` for release summaries; reserve decision records for choices
whose rationale will matter to later contributors.

## Current Decisions
- `binary_storage_guidance.md` — policy for large, protected, and generated
  binary artefacts.
