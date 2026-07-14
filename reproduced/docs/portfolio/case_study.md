# Case Study

Reference index: [`reproduced/docs/README.md`](../README.md)

## The Problem

University students make decisions about drinking in a social environment where "normal" behaviour is inferred from friends, flatmates, and the broader residence community. The central question: how much of student drinking behaviour is driven by personal tendencies, how much by who they become friends with (social selection), and how much by influence from existing friends over time (social influence)?

This matters because interventions are designed differently depending on the answer. If behaviour change is mostly selection — people choosing similar friends — then interventions should target early grouping conditions. If change is mostly influence — friends shifting each other after ties form — then interventions should target network-level diffusion and timing.

The study enrolled 255 first-year students (68% of 375 invited) in a single UK residence hall (8 blocks, 45 flats) across six survey waves from September 2022 to October 2023. Wave 6 included 223 respondents, giving 87% final-wave retention; participation across follow-up waves ranged from 84% to 88%. Eight supplied no alcohol-use or social-network data, so the Chapter 7 SAOM focuses on 247 full participants; 238–244 contributed observations in each modelled wave. Repeated friendship nominations (up to 10 per wave via the Important People Instrument), AUDIT-C drinking scores (0–12), and perceived norms at peer and global levels allow the pipeline to model behaviour as a dynamic social process rather than a cross-sectional correlation.

## What the Pipeline Reproduces

### The Freshers' spike and its aftermath

Mean AUDIT-C surged from 4.8 (pre-university baseline) to 7.4 in the first month, then gradually declined to 6.4–6.7 across subsequent waves. At the October peak, 80% of students reported monthly binge drinking and 60% reported weekly binge drinking — rates far exceeding comparable US studies (~28% in the SPARC cohort). The pipeline's Chapter 4 data preparation stage captures this trajectory and feeds it into downstream models.

### Norm misperceptions drive consumption — but the referent shifts

Network Autocorrelation Models (NAM) in Chapter 5 estimate how misperceptions of peer drinking relate to personal consumption while controlling for network dependence. The key finding is a referent shift across the academic year:

| Time period | Global misperception β | Peer misperception β |
|---|---|---|
| Time 1 (Oct) | 0.541 (p<0.001) | 0.045 (n.s.) |
| Time 2 (Mar) | 0.547 (p<0.001) | 0.124 (p<0.05) |
| Time 3 (Oct) | 0.246 (p<0.01) | 0.192 (p<0.01) |

Early in the year, overestimating how much the typical resident drinks (global misperception) is the dominant predictor. By year-end, local peer-level misperception gains significance as students form stable friendship clusters and calibrate their reference frame. Students consistently overestimated typical-resident drinking by 1.5–1.7 AUDIT-C points across all waves; peer-level misperception hovered near zero.

Chapter 6 extends this to injunctive norms — perceived approval of risky drinking. Overestimation of peer approval was prevalent but did not significantly predict consumption, a null finding that is itself informative for intervention design.

### Peer influence without social selection

The Chapter 7 SAOM jointly models network tie change and behaviour change, separating selection from influence. The headline result:

- Average similarity effect: β=1.88 (SE=0.68, p<0.01), standardised OR=1.17 (95% CI: 1.05–1.31). Students were 17% more likely to adjust their drinking one unit closer to their friends' average than to maintain their current level.
- Social selection on AUDIT-C: not significant (ego β=−0.04, p=0.18; alter β=0.03, p=0.35; similarity β=−0.08, p=0.88). Drinking habits did not predict friendship formation.

The network itself is shaped by structural forces: reciprocity (β=2.70, p<0.001), transitive triplets (β=0.78, p<0.001), flatmate proximity (β=0.76, p<0.001), and blockmate proximity (β=0.80, p<0.001). Because network measurement began one month after arrival, the model cannot test drinking-based selection during the first days and weeks when many relationships formed.

Convergence was good: overall max convergence ratio ≤0.10, Jaccard indices 0.66–0.81 between consecutive waves.

## Honest Limitations

- Behaviour goodness-of-fit is poor across all periods (p<0.001). The model poorly captures the zero-heavy, non-evenly distributed AUDIT-C outcome, especially non-drinker dynamics.
- Network data collection began in October (Wave 2), missing the first days and weeks when drinking-based friendship selection may have occurred.
- Single-hall design in South Yorkshire. UK drinking culture differs substantially from US campus norms — 80% monthly binge drinking at the October peak — so findings may not generalise to other residential settings or universities with different drinking cultures.
- 68% response rate (255 of 375 invited). Missing network data could bias tie-formation estimates.
- Self-reported alcohol data introduce possible social-desirability and recall bias despite confidentiality, pseudonymisation, and REDCap access controls.

## Reproducibility Engineering

The pipeline is not just the models — it is the run contract around them.

- Config-driven orchestration: paths, chapter toggles, seeds, and output contracts are centralised in `reproduced/config/thesis.yml`. No magic constants in scripts.
- Deterministic defaults: Chapters 4–6 are deterministic for fixed inputs. Chapter 7 uses RNG seed 2022 with n3=10,000 but may show small cross-platform drift due to BLAS/LAPACK differences.
- Fail-fast behaviour: no placeholder coefficients, no synthetic fallback outputs. If RSiena is not installed, if `list_by_wave.RData` is missing, or if any upstream dependency is absent, the pipeline `stop()`s immediately.
- Explicit data modes: default is real staged data; proxy mode must be requested (`SAND_DATA_MODE=proxy`).
- Validation dashboard: per-chapter pass/fail comparing pipeline outputs to thesis reference values with explicit tolerances.
- Reproducibility manifest: SHA-256 checksums for declared inputs and outputs, RNG seeds, and execution timestamps.
- Privacy guard: scans all portfolio outputs for identifier leakage before packaging.

## What This Work Demonstrates

1. **Pipeline implementation:** consolidated chapter orchestration under a consistent Make/config contract with deterministic seeds and checksum verification.
2. **SAOM debugging and stabilisation:** resolved RSiena batch-mode crashes (tkvars/tcltk), disabled unstable PSOCK parallelisation, maintained thesis-aligned wave selection (2, 4, 5, 6) and target effects.
3. **Proxy data workflow:** integrated realistic proxy generation for reproducible demos with explicit privacy guardrails.
4. **Portfolio packaging:** generated validation dashboard, data dictionary (105 variables), reproducibility manifest, and interactive network explorer with stats panels and interpretive captions.
5. **Narrative translation:** authored reviewer-facing documents that connect statistical results to actionable insight — specificity over vagueness, limitations acknowledged honestly.

## Scope Note

The repository maintains Chapters 4–7 code. The verified public fast path is
Chapters 4–6; Chapter 7 is included as a partial, long-running manual
diagnostic. Experimental Chapter 8 intervention code is disabled by default
and is not part of any verified public workflow.
