# SAND Study Book

Reference index: [`reproduced/docs/README.md`](../README.md)

Public preview generated on 2026-07-08. This document describes the Sheffield
Alcohol and Network Dynamics (SAND) study, the structure of its longitudinal
network-behaviour data, and the current public-release workflow. It is written
as a study book: enough detail for a reader to understand the design, variables,
data layers, missing-data issues, and reproducibility contract before requesting
or using any protected data.

The variable inventory below is based on the proxy-schema dictionary generated
from `reproduced/data/proxy/list_by_wave.RData` on 2026-02-12 and cross-checked
against the thesis methods chapters. Variable names and transformations mirror
the SAND workflow, but proxy-observed ranges should not be read as empirical
disclosure about real participants. Where the proxy schema carries columns for
all waves, the dictionary distinguishes schema compatibility from the
substantive thesis waves used for analysis.

## Contents

- [How To Use This Study Book](#how-to-use-this-study-book)
- [Study Overview](#study-overview)
- [Study Design](#study-design)
- [Data Access And Release Modes](#data-access-and-release-modes)
- [Data Layers](#data-layers)
- [Network Boundary And Important-Peer Nominations](#network-boundary-and-important-peer-nominations)
- [Missing Data And Quality Notes](#missing-data-and-quality-notes)
- [Reproducibility Workflow](#reproducibility-workflow)
- [Compact Data Dictionary](#compact-data-dictionary)
- [Derived Measures](#derived-measures)
- [Public Release Checklist](#public-release-checklist)
- [Documentation Lineage](#documentation-lineage)

## How To Use This Study Book

This document has two audiences.

Researchers without SAND data access can use it to understand what the study
measured, which variables exist, how the network boundary was defined, and what
kind of proxy or synthetic material can be made public. This is the right mode
for portfolio pages, data-access conversations, and reproducibility review.

Researchers with approved access can use it as an orientation map before running
the pipeline. The authoritative run contract remains
[`REPRODUCIBLE_PIPELINE.md`](../../REPRODUCIBLE_PIPELINE.md); this study book
summarises the data structure and points to the chapter scripts that generate
analysis-ready files.

The public preview should not include raw REDCap exports, free-text responses,
contact details, real participant identifiers, real edge lists, or any table
that could identify a small group of participants by combining attributes.

## Study Overview

SAND is a longitudinal study of social networks, alcohol use, alcohol-related
consequences, and norm perceptions among first-year university students living
in one UK residence hall. The thesis-facing dataset follows 255 enrolled
students out of 375 invited residents across six survey waves from September
2022 to October 2023.

The study was designed to answer three linked questions:

1. How do first-year students' drinking behaviours change after moving into a
   residential university environment?
2. How do perceived norms about peer and typical-resident drinking relate to
   personal alcohol consumption?
3. Do students form or maintain important-peer ties because they drink similarly,
   or do they become more similar in drinking after those ties form?

The data combine repeated survey measures with sociocentric important-peer
nominations. This makes it possible to model both individual trajectories and
the evolving social graph in which those trajectories occur.

## Study Design

| Feature | SAND design |
| --- | --- |
| Setting | One university-managed residence hall in South Yorkshire, UK |
| Invited population | 375 first-year residents |
| Enrolled sample | 255 students |
| Chapter 7 analytic cohort | 247 full participants; 238–244 observed per modelled wave |
| Survey waves | 6 waves, September 2022 to October 2023 |
| Network nominations | Up to 10 important-peer nominations per participant from Wave 2 onward |
| Behaviour outcome | AUDIT-C score, range 0-12 |
| Consequence measures | BYAACQ alcohol-consequence items, including the passing-out item used in Chapter 6 |
| Norm constructs | Descriptive and injunctive norm perceptions at global and peer levels |
| Main models | Network autocorrelation models and stochastic actor-oriented models |
| SAOM waves | Waves 2, 4, 5, and 6, with baseline covariates from Wave 1 |

### Wave Map

| Wave | Fieldwork timing | Downstream use |
| --- | --- | --- |
| Wave 1 | September 2022 baseline | Baseline covariates, pre-university alcohol measures, Chapter 4 preparation |
| Wave 2 | October 2022 follow-up | First network measurement; Chapter 5/6 Time 1; Chapter 7 SAOM start |
| Wave 3 | November 2022 follow-up | Chapter 4 longitudinal preparation and QA |
| Wave 4 | December 2022 follow-up | Chapter 7 SAOM |
| Wave 5 | March 2023 follow-up | Chapter 5/6 Time 2; Chapter 7 SAOM |
| Wave 6 | October 2023 follow-up | Chapter 5/6 Time 3; Chapter 7 SAOM end |

Chapter 5 and Chapter 6 use reduced analysis time points labelled Time 1, Time
2, and Time 3, corresponding to Waves 2, 5, and 6. Chapter 7 SAOM analyses use
network Waves 2, 4, 5, and 6, with Wave 1 providing baseline covariates. The
exact thesis-aligned wave choices are implemented in
[`00_build_network_arrays_base.R`](../../analyses/chapter7_saom/scripts/00_build_network_arrays_base.R).

## Data Access And Release Modes

SAND has three practical data modes.

| Mode | What it contains | Intended use | Public? |
| --- | --- | --- | --- |
| Real staged data | Protected analysis bundle assembled from controlled REDCap CSV exports, especially `list_by_wave.RData` | Reproduce thesis analyses after approved local staging | No |
| Proxy data | Synthetic structure-compatible data in `reproduced/data/proxy/` | Run the workflow, demonstrate schemas, exercise privacy and validation tooling | Yes, with clear labelling |
| Public preview | Study book, generated dictionary, schema summaries, proxy outputs, network screenshots or proxy-safe visualisations | Explain infrastructure and invite reproducible review without disclosing participant data | Yes |

The real data are never committed. They are staged manually into
`reproduced/data/raw/`. Proxy mode is explicit: generate proxy inputs with
`make proxy-data`, then run with `SAND_DATA_MODE=proxy`.

Public material should use proxy data, aggregate outputs, or schema-level
metadata. For any future data request workflow, the request should specify the
research question, requested variables, analysis plan, access controls, and
whether real network data are required.

## Data Layers

| Layer | Canonical location | Description |
| --- | --- | --- |
| Protected analysis input | `reproduced/data/raw/list_by_wave.RData` | Protected list of wave-level analysis frames assembled downstream of controlled REDCap CSV exports |
| Optional raw CSVs | `reproduced/data/raw/participants.csv`, `reproduced/data/raw/outcomes.csv` | Optional convenience files for hashing and smoke checks |
| Proxy export | `reproduced/data/proxy/list_by_wave.RData` | Structure-compatible synthetic data for demos and tests |
| Proxy schema | `reproduced/data/proxy/list_by_wave_schema.csv` | Wave-by-column storage-mode inventory |
| Chapter 4 outputs | `reproduced/outputs/chapter4/` | Prepared longitudinal data, QA reports, manifests |
| Chapter 5 outputs | `reproduced/outputs/chapter5/` | Descriptive-norm NAM inputs, summaries, comparisons |
| Chapter 6 outputs | `reproduced/outputs/chapter6/` | Injunctive-norm trajectories, summaries, validation |
| Chapter 7 outputs | `reproduced/outputs/chapter7/` | SAOM inputs, fitted objects, tables, figures, diagnostics |
| Portfolio outputs | `reproduced/outputs/portfolio/` | Data dictionary, reproducibility manifest, validation dashboard |

The key staged object is `list_by_wave.RData`, which must contain a list named
`list_by_wave`. Each element represents one survey wave.

## Network Boundary And Important-Peer Nominations

The network boundary is the first-year residence hall cohort. Participants could
nominate up to 10 people who had been important to them in the past month,
regardless of whether they liked them. Downstream scripts convert these
important-peer nominations into directed sociocentric network arrays aligned to
the analysis waves. Some legacy variable names and script comments still use
`friend` as shorthand for nominated important peers.

Important points for analysis:

- Nominations are directed. A nomination from participant A to participant B is
  not automatically reciprocal.
- The pipeline keeps nomination slot information through `which_friendid`.
- SAOM preprocessing uses a stable participant ordering so adjacency matrices
  and behaviour arrays align.
- Network nominations begin at Wave 2, after students have already had time to
  meet each other, so the earliest tie-formation period is not fully observed.

## Missing Data And Quality Notes

Missingness can arise from non-response, skipped survey sections, no nomination
being made, or the nominated participant not having available self-report data
for that wave. Alcohol modules can also create structural missingness where
follow-up questions are not applicable to non-drinkers.

The current public study book does not expose individual missingness patterns.
For real-data runs, Chapter 4 QA outputs should be used to inspect:

- participation by wave,
- nomination volume by wave,
- impossible or out-of-range values,
- participant alignment across waves,
- generated network-array dimensions,
- checksums for staged inputs and generated outputs.

Known limitations to carry into analysis are documented in the portfolio
[`case_study.md`](case_study.md), including self-report bias, a single-hall
setting, and behaviour goodness-of-fit limitations in the SAOM.

## Reproducibility Workflow

The SAND reproduction workspace is designed around a Make/config contract.

| Workflow element | Implementation |
| --- | --- |
| Config | `reproduced/config/thesis.yml` |
| Entrypoint | `make all` for Chapters 4-7 |
| Proxy generation | `make proxy-data` |
| Real/proxy switch | `SAND_DATA_MODE`, or `data.mode` in config |
| Validation | Chapter-specific validation scripts plus dashboard |
| Checksums | Input and output hashes in logs/manifests |
| Privacy guard | `scripts/portfolio/check_privacy.R` scans Markdown/HTML outputs |
| Generated dictionary | `scripts/portfolio/generate_data_dictionary.R` |

The workflow fails fast when required real data are missing. It should not
silently generate placeholders or treat proxy results as real findings.

## Compact Data Dictionary

This compact dictionary is intended for public preview. It collapses repeated
friend-slot variables into patterns so the structure is readable. Running
`make DATA_MODE=proxy portfolio` generates the expanded dictionary at
`reproduced/outputs/portfolio/data_dictionary.md`; generated outputs are not
stored in the release source tree.
The `Waves` column below reports substantive thesis use where that differs from
the proxy schema. The proxy bundle may include compatibility columns outside
those substantive waves so that downstream scripts can run in proxy mode.

### Identifiers

| Variable | Type | Expected encoding | Description | Waves |
| --- | --- | --- | --- | --- |
| `redcap_survey_identifier` | integer | Pseudonymous participant key | Unique participant identifier used for within-study linkage | 1-6 |
| `redcap_event_name` | character | `wave1_arm_1` to `wave6_arm_1` | REDCap event label for the survey wave | 1-6 |

### Demographics And Residence

| Variable | Type | Expected encoding | Description | Waves |
| --- | --- | --- | --- | --- |
| `age` | numeric | Years | Participant age at survey | 1-6 |
| `sex` | integer | 0/1 coding in analysis data | Sex variable used in model covariates | 1-6 |
| `ethnicity` | integer | Collapsed categorical code | Ethnicity category used in analysis data | 1-6 |
| `majority_status` | integer | 0/1 | Composite majority-status covariate used in Chapter 7 where available; derived from baseline demographic fields in real-data runs | 1-6 |
| `residence_cluster` | integer | Proxy residence grouping | Proxy-compatible residence grouping; real-data proximity covariates are derived from block and flat fields | 1-6 |
| `number_block` | integer or character | Residence block code | Stable block membership used to construct the blockmate dyadic covariate; required by the maintained Chapter 4 workflow | 1-6 |
| `number_flat` | integer or character | Residence flat code | Stable flat membership used to construct the flatmate dyadic covariate; required by the maintained Chapter 4 workflow | 1-6 |

### Alcohol Use And Consequences

| Variable | Type | Expected range | Description | Waves |
| --- | --- | --- | --- | --- |
| `q1` | numeric | 0-4 | AUDIT-C item 1: drinking frequency | 1-6 |
| `q2` | numeric | 0-4 | AUDIT-C item 2: typical number of drinks per occasion | 1-6 |
| `q3` | numeric | 0-4 | AUDIT-C item 3: heavy episodic drinking frequency | 1-6 |
| `audit_score` | numeric | 0-12 | AUDIT-C composite score, `q1 + q2 + q3` | 1-6 |
| `byaacq_6` | numeric | 0-6 in proxy/schema checks | BYAACQ-derived passing-out field used to construct the Chapter 6 alcohol-induced blackout outcome; not the full BYAACQ total | 1-6 |

### Important-Peer Nominations

| Variable | Type | Expected encoding | Description | Waves |
| --- | --- | --- | --- | --- |
| `friend_number` | numeric | 0-10 | Number of important-peer nominations made by the participant | 2-6; proxy schema may include Wave 1 |
| `which_friendid` | integer | 1-10 | Nomination slot index | 2-6 |
| `nomination` | integer | Pseudonymous participant key | Identifier of the nominated important peer | 2-6 |

### Norm Perceptions

| Variable pattern | Type | Encoding note | Description | Waves |
| --- | --- | --- | --- | --- |
| `inno1_self` | numeric | Thesis approval scale; proxy range is synthetic | Participant's own approval of not drinking in social settings | 1-6 |
| `inno2_self` | numeric | Thesis approval scale; proxy range is synthetic | Participant's own approval of binge drinking | 1-6 |
| `inno3_self` | numeric | Thesis approval scale; proxy range is synthetic | Participant's own approval of drinking enough to pass out | 1-6 |
| `deno1_friend_0` | integer | Adapted AUDIT-C item | Perceived drinking frequency for a typical resident | 2-6; proxy schema may include Wave 1 |
| `deno3_friend_0` | integer | Adapted AUDIT-C item | Perceived heavy episodic drinking frequency for a typical resident | 2-6; proxy schema may include Wave 1 |
| `deno4_friend_0` | integer | Additional descriptive alcohol-use item | Perceived drunkenness frequency for a typical resident | 2-6; proxy schema may include Wave 1 |
| `inno1_friend_0` | integer | Thesis approval scale; proxy range is synthetic | Perceived approval of not drinking by a typical resident | 2-6; proxy schema may include Wave 1 |
| `inno2_friend_0` | integer | Thesis approval scale; proxy range is synthetic | Perceived approval of binge drinking by a typical resident | 2-6; proxy schema may include Wave 1 |
| `inno3_friend_0` | integer | Thesis approval scale; proxy range is synthetic | Perceived approval of drinking enough to pass out by a typical resident | 2-6; proxy schema may include Wave 1 |
| `deno1_friend_[1-10]` | integer | Adapted AUDIT-C item | Perceived drinking frequency for nominated important-peer slot 1-10 | 2-6; proxy schema may include Wave 1 |
| `deno3_friend_[1-10]` | integer | Adapted AUDIT-C item | Perceived heavy episodic drinking frequency for nominated important-peer slot 1-10 | 2-6; proxy schema may include Wave 1 |
| `deno4_friend_[1-10]` | integer | Additional descriptive alcohol-use item | Perceived drunkenness frequency for nominated important-peer slot 1-10 | 2-6; proxy schema may include Wave 1 |
| `inno1_friend_[1-10]` | integer | Thesis approval scale; proxy range is synthetic | Perceived approval of not drinking by nominated important-peer slot 1-10 | 2-6; proxy schema may include Wave 1 |
| `inno2_friend_[1-10]` | integer | Thesis approval scale; proxy range is synthetic | Perceived approval of binge drinking by nominated important-peer slot 1-10 | 2-6; proxy schema may include Wave 1 |
| `inno3_friend_[1-10]` | integer | Thesis approval scale; proxy range is synthetic | Perceived approval of drinking enough to pass out by nominated important-peer slot 1-10 | 2-6; proxy schema may include Wave 1 |

### Derived Peer And Misperception Measures

| Variable | Type | Description | Waves |
| --- | --- | --- | --- |
| `actual_audit_score_peer` | numeric | Mean AUDIT-C score among nominated important peers | 2-6; proxy schema may include Wave 1 |
| `actual_deno1_peer` | numeric | Mean actual descriptive item 1 across nominated important peers | 2-6; proxy schema may include Wave 1 |
| `actual_deno3_peer` | numeric | Mean actual descriptive item 3 across nominated important peers | 2-6; proxy schema may include Wave 1 |
| `actual_deno4_peer` | numeric | Mean actual descriptive item 4 across nominated important peers | 2-6; proxy schema may include Wave 1 |
| `actual_inno1_peer` | numeric | Mean actual approval of not drinking across nominated important peers | 2-6; proxy schema may include Wave 1 |
| `actual_inno2_peer` | numeric | Mean actual approval of binge drinking across nominated important peers | 2-6; proxy schema may include Wave 1 |
| `actual_inno3_peer` | numeric | Mean actual approval of drinking enough to pass out across nominated important peers | 2-6; proxy schema may include Wave 1 |
| `deno_audit_peer` | numeric | Aggregated perceived peer AUDIT-C score | 2-6; proxy schema may include Wave 1 |
| `deno1_peer` | numeric | Aggregated perceived peer drinking-frequency item | 2-6; proxy schema may include Wave 1 |
| `deno3_peer` | numeric | Aggregated perceived peer heavy-episodic-drinking item | 2-6; proxy schema may include Wave 1 |
| `deno4_peer` | numeric | Aggregated perceived peer drunkenness item | 2-6; proxy schema may include Wave 1 |
| `inno1_peer` | numeric | Aggregated perceived peer approval of not drinking | 2-6; proxy schema may include Wave 1 |
| `inno2_peer` | numeric | Aggregated perceived peer approval of binge drinking | 2-6; proxy schema may include Wave 1 |
| `inno3_peer` | numeric | Aggregated perceived peer approval of drinking enough to pass out | 2-6; proxy schema may include Wave 1 |
| `misperception_audit_score_peer` | numeric | Perceived peer AUDIT-C minus actual peer mean AUDIT-C | 2-6; proxy schema may include Wave 1 |
| `misperception_q1_peer` | numeric | Perceived peer drinking-frequency item minus actual peer mean | 2-6; proxy schema may include Wave 1 |
| `misperception_q2_peer` | numeric | Perceived peer quantity/second descriptive item minus actual peer mean | 2-6; proxy schema may include Wave 1 |
| `misperception_q3_peer` | numeric | Perceived peer heavy-episodic-drinking item minus actual peer mean | 2-6; proxy schema may include Wave 1 |
| `misperception_inno1_peer` | numeric | Perceived peer approval of not drinking minus actual peer mean | 2-6; proxy schema may include Wave 1 |
| `misperception_inno2_peer` | numeric | Perceived peer approval of binge drinking minus actual peer mean | 2-6; proxy schema may include Wave 1 |
| `misperception_inno3_peer` | numeric | Perceived peer approval of drinking enough to pass out minus actual peer mean | 2-6; proxy schema may include Wave 1 |

## Derived Measures

The derived variables are produced in the Chapter 4 preparation workflow and
then reused in Chapters 5 and 6.

| Derived family | Formula / interpretation |
| --- | --- |
| `actual_*_peer` | Mean of the corresponding measure across nominated important peers |
| `deno*_peer` | Participant's aggregated perception of important-peer descriptive norms |
| `inno*_peer` | Participant's aggregated perception of important-peer injunctive norms |
| `misperception_*_peer` | Participant's perceived important-peer value minus the actual important-peer mean |
| `audit_score` | `q1 + q2 + q3` |

For public release, derived formulas are safer than row-level derived values.
They show how the analysis works without disclosing real individual outcomes or
real network ties.

## Public Release Checklist

Before publishing a SAND study-book or dictionary page, check:

- No real participant identifiers, names, emails, phone numbers, or free text.
- No real edge lists or matrices unless an approved disclosure-control and
  governance process authorises their release.
- No small-cell demographic summaries that could identify a participant.
- Proxy outputs are labelled as proxy or synthetic wherever they appear.
- Validation reports distinguish proxy-mode smoke checks from real-data thesis
  validation.
- Checksums and manifests describe files without exposing protected content.
- Any public data sample uses release-specific pseudonymous keys that cannot be
  linked across restricted datasets.
- The page links to documentation, code, or generated schema files only after
  they have passed the privacy scanner.

## Documentation Lineage

This study-book structure borrows three documentation patterns from mature
longitudinal network projects:

1. A plain study overview before the codebook, so readers understand the
   scientific setting before seeing variable names.
2. A data-access section that separates public schema/proxy material from
   restricted research data.
3. A codebook section that explains wave storage, missingness, network
   variables, and derived measures.

The public-facing release model is especially close to the Swiss StudentLife
Study's documentation pattern: a high-level study page, a detailed codebook,
and a clear data-access protocol for sensitive network data. See the ETH Social
Networks Lab page at <https://sn.ethz.ch/research/studentlife.html> and the
StudentLife codebook at <https://codebook.sn.ethz.ch/>.

The Brown SQUAD and 21RISING materials reviewed during preparation were useful
as private structural references for codebook organisation, but they are not
source material for this public preview and should not be republished here.
