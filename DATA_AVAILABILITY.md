# Data Availability

## Public Materials

The public repository contains analysis code, configuration, environment definitions, documentation, a schema inventory, and a generator for synthetic proxy data. These materials support inspection and execution without distributing participant records.

## Protected Participant Data

The repository does not distribute the SAND participant dataset. It contains sensitive longitudinal alcohol-use, demographic, residential, and social-network information. Pseudonymisation does not remove the disclosure risk created by a bounded social graph and repeated measurements.

The analysis expects approved users to stage the protected analysis-facing wave
bundle locally under `reproduced/data/raw/`. That bundle is assembled downstream
of controlled REDCap CSV exports. Git ignores the directory. The public
repository does not promise a data-access route because the data controller,
consent conditions, and review procedure have not been confirmed.

## Ethics and Governance

The University of Sheffield approved the project on ethics grounds on 8 June
2022 under reference `046766`. The approval was administered by the School of
Medicine and Population Health and covered the submitted research ethics
application, participant information sheet, and participant consent form;
subsequent amendments are recorded in the institutional approval letter.

This reference establishes that the study received ethics approval. It does
not, by itself, authorise public release of participant data or establish a
data-access route. The study-specific consent terms and institutional
data-controller process must be checked before any future data sharing.

## Synthetic Proxy Data

`make proxy-data` generates a fully synthetic, structure-compatible dataset. The generator uses published study-level design parameters and modelling requirements to exercise the pipeline. It does not transform, perturb, or anonymise participant rows.

Proxy outputs serve software testing and demonstration only. They do not reproduce the empirical results and must not support substantive claims about the study population.

## Aggregate Results

The principal aggregate findings in the README and its figures were cross-checked against thesis Chapters 4–7 on 13 July 2026. This was a source cross-check, not a new protected-data analysis run. Any later result added to the public materials must be traced to the thesis or a validated real-data output and reviewed for disclosure risk.

## Access Requests

No public access procedure is currently offered through this repository. Any future request for protected data must follow the confirmed institutional governance route and may require ethics review, a data-sharing agreement, secure storage, and limits on onward sharing.
