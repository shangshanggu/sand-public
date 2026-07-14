# Protected-Data Benchmark Targets

This file records thesis-derived NAM coefficients used to validate Chapter 5
and Chapter 6 real-data runs. It contains aggregate model targets, not
participant-level data. Proxy runs use these values only to report expected
differences; synthetic inputs are not expected to reproduce them.

On February 8, 2026, both Chapter 5 and Chapter 6 were switched to live `sna::lnam()` estimation. The reproduced pipeline uses the same raw data (`list_by_wave.RData`) and methodology as the legacy analysis environment. Two fixes were applied to match the thesis exactly:

1. **Zero-friend peer coding** (`compute_peer_mean` in `01_data_preparation_norms.R`): participants with `friend_number == 0` get `peer_misperception = 0`; participants with friends whose friends all have missing data keep `NaN` (dropped as incomplete).
2. **Further imputation mean source** (`prepare_model_inputs` in `02_estimate_nam_models.R`): the mean for single-NA imputation is computed from the flagged subset only (matching the legacy `filter(further_imputation==1) %>% mutate_at(...)` pipeline).
3. **Adjacency normalisation** (`build_adjacency_matrix`): uses `diag(1/colSums) %*% adj` (legacy normalisation), not standard column-normalisation.

These fixes produce thesis-matching sample sizes (218/213/215 for Time 1/2/3) and coefficients that agree with the thesis tables to 3 decimal places.

## Chapter 5 - NAM Regression Expectations

The Chapter 5 validator checks reproduced regression coefficients against these
curated values. The JSON block provides machine-readable targets consumed by
`reproduced/analyses/chapter5_descriptive_norms/scripts/04_validate_nam_results.R`.

```json
{
  "chapter5_nam_expectations": {
    "Time 1": {
      "global_misperception": {"estimate": 0.5406350685516100, "tolerance": 1e-04},
      "peer_misperception": {"estimate": 0.0450078688256556, "tolerance": 1e-04}
    },
    "Time 2": {
      "global_misperception": {"estimate": 0.5465641671658140, "tolerance": 1e-04},
      "peer_misperception": {"estimate": 0.1236031201406040, "tolerance": 1e-04}
    },
    "Time 3": {
      "global_misperception": {"estimate": 0.2458588130280870, "tolerance": 1e-04},
      "peer_misperception": {"estimate": 0.1923528467993540, "tolerance": 1e-04}
    }
  }
}
```

For reference, the thesis tables report (rounded to 3dp): Time 1 global=0.541, peer=0.045; Time 2 global=0.547, peer=0.124; Time 3 global=0.246, peer=0.192. The reproduced values match these exactly at 3dp.

## Chapter 6 - Injunctive Norms NAM Expectations

The thesis Chapter 6 presents three "Combined" model tables (Tables 2a, 2b,
passing-out) each covering Time 1 / Time 2 / Time 3. The key finding is that
neither global-level nor peer-level misperceptions of injunctive norms are
statistically significant predictors of drinking behaviour.

On February 8, 2026, Chapter 6 was reverse-engineered to exact match using the
same three fixes as Chapter 5:

1. **Adjacency normalisation**: `diag(1/colSums) %*% adj` (legacy normalisation, not row-normalisation).
2. **Further imputation mean source**: mean computed from the flagged subset only (rows with exactly 1 misperception NA).
3. **Covariate order**: `age, sex, if_white, friend_number, audit_score_previous, misp_peer, misp_global`.

These fixes produce coefficients that match the thesis tables to 3 decimal places across all 63 checks (3 outcomes × 3 time points × 7 terms).

The validation script
`reproduced/analyses/chapter6_injunctive_norms/scripts/05_validate_injunctive_results.R`
consumes the JSON block below.

```json
{
  "chapter6_injunctive_expectations": {
    "drinker": {
      "Time 1": {
        "global_misperception": {"estimate": -0.0006098729100971, "std_error": 0.0133792591422404, "tolerance": 1e-04},
        "peer_misperception":   {"estimate":  0.0053651150812794, "std_error": 0.0124731328735989, "tolerance": 1e-04},
        "audit_score_previous": {"estimate":  0.0319436533337221, "std_error": 0.0060137765982060, "tolerance": 1e-04},
        "age":                  {"estimate":  0.0321924403989766, "std_error": 0.0023852526353521, "tolerance": 1e-04},
        "sex":                  {"estimate":  0.0245025502061012, "std_error": 0.0310877392347986, "tolerance": 1e-04},
        "if_white":             {"estimate":  0.1746238849217206, "std_error": 0.0410877135223252, "tolerance": 1e-04},
        "friend_number":        {"estimate":  0.0107231002732782, "std_error": 0.0064277207032884, "tolerance": 1e-04}
      },
      "Time 2": {
        "global_misperception": {"estimate": -0.0146679667347382, "std_error": 0.0167410366154527, "tolerance": 1e-04},
        "peer_misperception":   {"estimate":  0.0044583468231462, "std_error": 0.0151738073550456, "tolerance": 1e-04},
        "audit_score_previous": {"estimate":  0.0463746803160318, "std_error": 0.0057777731137216, "tolerance": 1e-04},
        "age":                  {"estimate":  0.0227777361798371, "std_error": 0.0030469795609813, "tolerance": 1e-04},
        "sex":                  {"estimate": -0.0410843022004992, "std_error": 0.0345823414234920, "tolerance": 1e-04},
        "if_white":             {"estimate":  0.1669720338142092, "std_error": 0.0475129060314510, "tolerance": 1e-04},
        "friend_number":        {"estimate": -0.0059445606577843, "std_error": 0.0090773390687329, "tolerance": 1e-04}
      },
      "Time 3": {
        "global_misperception": {"estimate":  0.0060400412866037, "std_error": 0.0140401053340129, "tolerance": 1e-04},
        "peer_misperception":   {"estimate": -0.0200912771785688, "std_error": 0.0129238933584768, "tolerance": 1e-04},
        "audit_score_previous": {"estimate":  0.0451256859228085, "std_error": 0.0051951451763735, "tolerance": 1e-04},
        "age":                  {"estimate":  0.0314123255690737, "std_error": 0.0025494544324587, "tolerance": 1e-04},
        "sex":                  {"estimate": -0.0418171509571785, "std_error": 0.0302095250538717, "tolerance": 1e-04},
        "if_white":             {"estimate":  0.1001198969152423, "std_error": 0.0427454987511965, "tolerance": 1e-04},
        "friend_number":        {"estimate": -0.0001088228407558, "std_error": 0.0092470851833962, "tolerance": 1e-04}
      }
    },
    "binge_drinker": {
      "Time 1": {
        "global_misperception": {"estimate": -0.0039104327072293, "std_error": 0.0163115343779859, "tolerance": 1e-04},
        "peer_misperception":   {"estimate": -0.0189018335633364, "std_error": 0.0159982583060028, "tolerance": 1e-04},
        "audit_score_previous": {"estimate":  0.0587972662083467, "std_error": 0.0076157526511591, "tolerance": 1e-04},
        "age":                  {"estimate":  0.0179666807166042, "std_error": 0.0029348358203158, "tolerance": 1e-04},
        "sex":                  {"estimate":  0.0431249361820027, "std_error": 0.0388068179363050, "tolerance": 1e-04},
        "if_white":             {"estimate":  0.2353129660746972, "std_error": 0.0522372569160542, "tolerance": 1e-04},
        "friend_number":        {"estimate":  0.0100427683728163, "std_error": 0.0082519040469997, "tolerance": 1e-04}
      },
      "Time 2": {
        "global_misperception": {"estimate": -0.0117870800427323, "std_error": 0.0157793476839865, "tolerance": 1e-04},
        "peer_misperception":   {"estimate":  0.0064780457982761, "std_error": 0.0145216554290672, "tolerance": 1e-04},
        "audit_score_previous": {"estimate":  0.0694723979131045, "std_error": 0.0067526611578262, "tolerance": 1e-04},
        "age":                  {"estimate":  0.0143762915521820, "std_error": 0.0035343730389094, "tolerance": 1e-04},
        "sex":                  {"estimate": -0.0527793975816575, "std_error": 0.0398203728076712, "tolerance": 1e-04},
        "if_white":             {"estimate":  0.1376782440032197, "std_error": 0.0552507258389336, "tolerance": 1e-04},
        "friend_number":        {"estimate": -0.0166025201530090, "std_error": 0.0103595088896339, "tolerance": 1e-04}
      },
      "Time 3": {
        "global_misperception": {"estimate": -0.0046185705971322, "std_error": 0.0160399001882185, "tolerance": 1e-04},
        "peer_misperception":   {"estimate":  0.0073348167663847, "std_error": 0.0167114229921184, "tolerance": 1e-04},
        "audit_score_previous": {"estimate":  0.0587849177908126, "std_error": 0.0066598352763855, "tolerance": 1e-04},
        "age":                  {"estimate":  0.0180200423454643, "std_error": 0.0032628993797160, "tolerance": 1e-04},
        "sex":                  {"estimate": -0.0377914397790905, "std_error": 0.0385281334956693, "tolerance": 1e-04},
        "if_white":             {"estimate":  0.1729146074645455, "std_error": 0.0552252640407912, "tolerance": 1e-04},
        "friend_number":        {"estimate":  0.0041890753930822, "std_error": 0.0117100213521220, "tolerance": 1e-04}
      }
    },
    "passing_out": {
      "Time 1": {
        "global_misperception": {"estimate":  0.0028147216164871, "std_error": 0.0182150135789181, "tolerance": 1e-04},
        "peer_misperception":   {"estimate":  0.0134994017505114, "std_error": 0.0191525461559167, "tolerance": 1e-04},
        "audit_score_previous": {"estimate":  0.0313439587309735, "std_error": 0.0095372535744797, "tolerance": 1e-04},
        "age":                  {"estimate": -0.0019153409585935, "std_error": 0.0038250785766306, "tolerance": 1e-04},
        "sex":                  {"estimate":  0.0296320068809921, "std_error": 0.0482692559625688, "tolerance": 1e-04},
        "if_white":             {"estimate": -0.0493130045782607, "std_error": 0.0654970218374238, "tolerance": 1e-04},
        "friend_number":        {"estimate":  0.0107473241946030, "std_error": 0.0090807082862199, "tolerance": 1e-04}
      },
      "Time 2": {
        "global_misperception": {"estimate":  0.0250381889963033, "std_error": 0.0194779710630547, "tolerance": 1e-04},
        "peer_misperception":   {"estimate": -0.0048078981602849, "std_error": 0.0187052344978249, "tolerance": 1e-04},
        "audit_score_previous": {"estimate":  0.0259850015055773, "std_error": 0.0079040874684844, "tolerance": 1e-04},
        "age":                  {"estimate": -0.0064805546036882, "std_error": 0.0041414296696724, "tolerance": 1e-04},
        "sex":                  {"estimate":  0.0032898413835878, "std_error": 0.0463096279528557, "tolerance": 1e-04},
        "if_white":             {"estimate": -0.0125517268920450, "std_error": 0.0644512258668281, "tolerance": 1e-04},
        "friend_number":        {"estimate":  0.0289934757562194, "std_error": 0.0110364588189998, "tolerance": 1e-04}
      },
      "Time 3": {
        "global_misperception": {"estimate": -0.0201669238425866, "std_error": 0.0190400844333173, "tolerance": 1e-04},
        "peer_misperception":   {"estimate": -0.0030932538811585, "std_error": 0.0196980600663076, "tolerance": 1e-04},
        "audit_score_previous": {"estimate":  0.0209207037088635, "std_error": 0.0078907358969961, "tolerance": 1e-04},
        "age":                  {"estimate":  0.0020010799138503, "std_error": 0.0039180282294612, "tolerance": 1e-04},
        "sex":                  {"estimate":  0.0405142918229099, "std_error": 0.0458161475633495, "tolerance": 1e-04},
        "if_white":             {"estimate": -0.0423079398837163, "std_error": 0.0653074053807075, "tolerance": 1e-04},
        "friend_number":        {"estimate": -0.0014283548178384, "std_error": 0.0111570247720472, "tolerance": 1e-04}
      }
    }
  }
}
```

For reference, the thesis tables report (rounded to 3dp):
- Drinker T1: global=-0.001, peer=0.005; T2: global=-0.015, peer=0.004; T3: global=0.006, peer=-0.020
- Binge T1: global=-0.004, peer=-0.019; T2: global=-0.012, peer=0.006; T3: global=-0.005, peer=0.007
- Passing out T1: global=0.003, peer=0.013; T2: global=0.025, peer=-0.005; T3: global=-0.020, peer=-0.003

The reproduced values match these exactly at 3dp.
