<div align="center">

# SAND

### Social Networks and Alcohol Use among First-Year University Students

*A Reproducible Analysis Pipeline for the Sheffield Alcohol and Network Dynamics Study*

---

**255 students** · **6 survey waves** · **87% final-wave retention** · **Chapters 4–7** · **Verified reproducible path: Chapters 4–6**

[Interactive Reference](https://sand.shangshanggu.com) · [Key Findings](#key-findings) · [Pipeline](#pipeline) · [Quick Start](#quick-start) · [Documentation](#documentation)

[Hosted CI: first public run passed](https://github.com/shangshanggu/sand-public/actions/runs/29366951200)

</div>

---

## Project Context

SAND is the product of my PhD research at the University of Sheffield (2021–2025). I designed the study, built the data-collection infrastructure from bare university VMs, recruited 255 participants, achieved 87% final-wave retention (223 of 255 baseline participants), conducted all analysis, and wrote the thesis. Every component in this repository — the REDCap architecture, the identity-separation protocol, the recruitment operations, the reproducible analysis pipeline, and this open-science release — was developed through my PhD research and subsequent Wellcome-funded Transition Grant position, with supervisory guidance.

## Overview

This repository contains the maintained code and documentation for rebuilding the quantitative analyses from the PhD thesis:

> **Social Networks and Alcohol Use among First-Year University Students: The Sheffield Alcohol and Network Dynamics Study**
>
> Shangshang Gu · Sheffield Centre for Health and Related Research · University of Sheffield · 2025

The study enrolled 255 first-year students across six survey waves in a UK residence hall (Sep 2022 – Oct 2023). Eight supplied no alcohol-use or social-network data, so the Chapter 7 SAOM focuses on 247 full participants; 238–244 contributed observations in each modelled wave. The protected-data workflow progresses from staged REDCap exports through network autocorrelation models of norm misperceptions to stochastic actor-oriented models that estimate social selection and social influence in drinking behaviour.

The **[interactive reference page](https://sand.shangshanggu.com)** provides the full technical documentation: study design, REDCap/MySQL infrastructure, identity separation, recruitment operations, data dictionaries, the analysis pipeline, and release verification.

## Key Findings

> **Evidence boundary:** The findings below are aggregate empirical results
> reported from the protected study data in the thesis and manuscripts. The
> public proxy workflow demonstrates executable software and output structure;
> it does not reproduce these estimates or provide participant-level evidence.

### Drinking Trends and Demographics (Chapter 4)

Mean AUDIT-C increased from 4.8 (pre-university) to 7.4 in the first month — 80% of students reported monthly binge drinking and 60% weekly. The spike gradually declined but never returned to baseline.

### Shifting Referents: Who Shapes Your Drinking? (Chapter 5)

Students consistently overestimated typical-resident drinking by 1.5–1.7 AUDIT-C points across all waves. The source of that influence shifted over the academic year — from "what does everyone drink?" to "what do my friends drink?":

| Time Period | Global Misperception | Peer Misperception | Interpretation |
|:-----------:|:--------------------:|:------------------:|----------------|
| Time 1 (Oct) | β = 0.541, p < 0.001 | β = 0.045, n.s. | Community norms dominate |
| Time 2 (Mar) | β = 0.547, p < 0.001 | β = 0.124, p < 0.05 | Peer influence emerges |
| Time 3 (Oct) | β = 0.246, p < 0.01 | β = 0.192, p < 0.01 | Both matter, peer gaining |

### Injunctive Norms: Approval ≠ Behaviour (Chapter 6)

Students overestimated peer approval of risky drinking (abstaining, binge, blackout scenarios), but this misperception did not significantly predict consumption. The title says it all: injunctive norm misperception does not drive drinking behaviours.

### Influence Without Selection (Chapter 7)

The SAOM separates two competing explanations for why friends drink similarly:

**Social selection** — do similar drinkers become friends? No. AUDIT-C similarity (β = −0.08, p = 0.88), ego (β = −0.04, p = 0.18), and alter (β = 0.03, p = 0.35) effects were all non-significant.

**Social influence** — do friends become similar drinkers? The fitted model found evidence of influence. The average similarity effect (β = 1.88, p < 0.01) translates to an odds ratio of 1.17 (95% CI: 1.05–1.31). Under the model, students were 17% more likely to adjust their drinking one unit closer to their friends' average than to maintain their current level.

| Effect | β | SE | p | Interpretation |
|--------|---:|---:|:---:|:--------|
| Average similarity | 1.88 | 0.68 | <0.01 | Friends converge in drinking |
| Reciprocity | 2.70 | 0.31 | <0.001 | Mutual ties strongly preferred |
| Transitive triplets | 0.78 | 0.12 | <0.001 | Friends-of-friends become friends |
| Flatmate proximity | 0.76 | 0.23 | <0.001 | Living together → friendship |
| Blockmate proximity | 0.80 | 0.22 | <0.001 | Same building → friendship |
| Density | −2.91 | 0.52 | <0.001 | Network is sparse and selective |

Convergence: max ratio ≤ 0.10 · Jaccard indices: 0.67, 0.81, 0.66

## Pipeline

| Ch | Title | Method | Status |
|:--:|-------|--------|:------:|
| 4 | A Novel Method for Longitudinal Social Network & Behaviour Data Collection Using REDCap | Data extraction, QA, imputation, network array construction | Verified |
| 5 | How Do First-Year Students (Mis)perceive Their Peers' Alcohol Use? | Network Autocorrelation Models (NAM) x 3 time periods | Verified; reproduces thesis to 3dp |
| 6 | Misperception of Injunctive Social Norms Does Not Drive Drinking Behaviours | NAM x 3 approval scenarios | Verified; reproduces thesis to 3dp |
| 7 | Social Selection & Influence: The Co-evolution of Networks & Alcohol Consumption | Stochastic Actor-Oriented Models (SAOM), Waves 2/4/5/6 | Implemented; converged on thesis data |
| 8 | Simulation of Potential Intervention Strategies | Experimental counterfactual SAOM scenarios | Experimental; disabled by default |

## Quick Start

### Proxy mode (no real data required)

```bash
make env                 # bootstrap the locked R environment
make verify-proxy-quick  # regenerate proxy data and verify Chapters 4–6

# Optional long Chapter 7 diagnostic (~2h):
make verify-proxy
```

### Real data mode

```bash
make env

# Stage required files:
#   reproduced/data/raw/list_by_wave.RData  (required)
#   reproduced/data/raw/participants.csv    (optional)
#   reproduced/data/raw/outcomes.csv        (optional)

make verify-real  # Ch.4 > Ch.5 > Ch.6 > Ch.7 plus empirical benchmarks
```

## Reproducibility

Chapters 4–6 are fully deterministic for fixed inputs and configuration. Chapter 7 (SAOM) may show small cross-platform numeric drift due to BLAS/LAPACK and compiler differences, even with fixed seeds (RNG seed: `2022`, RSiena n3: `10000`).

The tracked scripts generate chapter-level logs, manifests, and selected checksums. Verification is mode-specific: proxy runs check execution and artifact structure while recording empirical differences as warnings; protected real-data runs enforce chapter benchmarks and Chapter 7 diagnostics.

## Known Limitations

> Intellectual honesty matters more than overselling.

| Limitation | Impact |
|------------|--------|
| Behaviour GoF is poor (p < 0.001) | The model poorly captures the zero-heavy, non-evenly distributed AUDIT-C outcome, especially non-drinker dynamics |
| Missing first-week data | Network measurement began one month after arrival; critical early formation period is unobserved |
| Single-hall design | One residence hall in South Yorkshire; UK binge rates (80%) far exceed US cohorts (~28%) |
| 68% response rate | Missing network data could bias tie-formation estimates |
| Self-reported alcohol data | Social desirability and recall error remain possible despite confidentiality and pseudonymisation measures |
| No study preregistration | This is a retrospective reproducibility release; it must not be presented as a preregistered analysis or as evidence that analytic choices were fixed before data inspection |

## Documentation

| Document | Purpose |
|----------|--------|
| **[Interactive Reference](https://sand.shangshanggu.com)** | **Study design, REDCap/MySQL infrastructure, identity separation, participant operations, data dictionaries, pipeline, and release verification** |
| [Pipeline Contract](reproduced/REPRODUCIBLE_PIPELINE.md) | Build instructions, data modes, failure modes |
| [Thesis Crosswalk](reproduced/docs/references/thesis_scope_and_crosswalk.md) | Maps thesis chapters to pipeline scripts |
| [Onboarding Guide](reproduced/docs/references/onboarding_pointer_map.md) | Real vs proxy data setup checklist |
| [Configuration Guide](reproduced/docs/references/configuration_guide.md) | Editing `thesis.yml` |
| [Documentation Hub](reproduced/docs/README.md) | Full documentation index |
| [Open Science Release Contract](reproduced/docs/references/open_science_release_contract.md) | Public-release scope and evidence |
| [Data Availability](DATA_AVAILABILITY.md) | What is public, protected, and synthetic |

## Citation

If you use the code, cite the repository using [`CITATION.cff`](CITATION.cff).

## Licensing

Code and synthetic proxy-data generator outputs use the [MIT License](LICENSE).
Documentation and diagrams use [CC BY 4.0](LICENSE-docs).

## Ethics and Governance

The University of Sheffield approved the study on ethics grounds on 8 June
2022 (reference `046766`), administered by the School of Medicine and
Population Health. This repository distributes no participant data and offers
no public data-access route. See [Data Availability](DATA_AVAILABILITY.md) for
the distinction between ethics approval, software publication, and any future
institutional data-sharing decision.

## Data Privacy

Real participant data are staged locally and are not committed or distributed. Proxy mode generates synthetic, structure-compatible data for public demonstrations. Proxy data are not anonymised participant data and must not be used for substantive inference. See [DATA_AVAILABILITY.md](DATA_AVAILABILITY.md).

## Acknowledgements

This research was funded by a Wellcome Trust Research and Training Support Grant (224 850/Z/21/Z) through the [PHEDS Doctoral Training Centre](https://www.pheds-dtc.ac.uk/) at the University of Sheffield. The funder had no role in study design, data collection, analysis, or the decision to publish.

**Supervisors:**

- **Professor John Holmes**, School of Medicine and Population Health, University of Sheffield
- **Professor Robin Purshouse**, School of Electrical and Electronic Engineering, University of Sheffield
- **Professor Paul Norman**, Department of Psychology, University of Sheffield

## Contact

**Shangshang Gu**
shangshanggu@gmail.com · [shangshanggu.com](https://shangshanggu.com)
