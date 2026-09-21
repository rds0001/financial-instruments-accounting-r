# financial-accounting-engine for R

`financialAccountingEngine` is the native R implementation of the independent
financial-accounting-engine. It provides 80 granular public functions for
financial-instrument classification, measurement, effective interest,
amortised cost, impairment, scenario-weighted expected credit losses,
modifications, derecognition, hedge accounting, journals, disclosures, model
monitoring, reconciliation, controls and audit lineage.

The package does not call Python, start subprocesses or require a source
checkout. Formula functions accept ordinary R vectors, matrices and lists.
Domain functions accept named canonical rows and tables. The complete workflow
uses those same public calculations and supports two fully synthetic offline
reference profiles.

## Installation

Install the checked source archive with base R:

```r
install.packages("financialAccountingEngine_0.1.0.tar.gz",
                 repos = NULL, type = "source")
```

After the GitHub repository is published, the development version can be
installed with `pak::pak("rds0001/financial-instruments-accounting-r")`.

## First calculation

```r
library(financialAccountingEngine)

component_ecl(
  marginal_pd = 0.02,
  ead = 100000,
  lgd = 0.40,
  eir = 0.05,
  year = 1
)

result <- run_dataset(load_reference_data("SMALL_SA_RETAIL_FIA"))
result$status
get_metrics(result)$closing_allowance
all(vapply(get_controls(result), function(x) x$status == "PASS", logical(1)))
```

## Public API

The 80 exported functions are organised around independently testable tasks:

| Area | Representative functions |
|---|---|
| Discounting and effective interest | `discount_factor()`, `effective_interest_rate()`, `amortisation_schedule()` |
| Classification and measurement | `assess_sppi()`, `classify_instrument()`, `initial_measurement()` |
| Staging and impairment | `assess_sicr()`, `assess_stage()`, `scenario_ecl()`, `provision_matrix_ecl()` |
| Modifications and derecognition | `process_modification()`, `assess_derecognition()`, `continuing_involvement()` |
| Hedge accounting | `validate_hedge_designation()`, `measure_hedge()`, `hedge_reserve_rollforward()` |
| Journals and disclosures | `build_journals()`, `aggregate_disclosures()`, `regulatory_bridge()` |
| Monitoring | `backtest_models()`, `compare_challenger()`, `reconcile_overlay()` |
| Reference workflow | `load_reference_data()`, `run_dataset()`, `get_metrics()`, `get_lineage()` |

Every export has installed Rd help, argument and return-value contracts,
references, runnable examples, and at least one direct test. Open the indexed
package overview with `?financialAccountingEngine`, search the installed
documentation with `??financialAccountingEngine`, or use
`help(package = "financialAccountingEngine")`. The four bundled vignettes,
`docs/USER_GUIDE.md`, and `tools/api_inventory.csv` describe the complete
surface from different perspectives.

## Data, decisions, and outputs

Rates and probabilities use decimal notation and time uses years. Monetary
inputs within one calculation use one currency unless an explicit FX mapping is
supplied. Missing judgements, approvals and model inputs are rejected rather
than silently defaulted. Outputs retain formula/rule identifiers and the full
workflow records input, policy, rule, code and resource hashes.

`SMALL_SA_RETAIL_FIA` and `MID_SIZE_UNIVERSAL_FIA` are deterministic,
synthetic profiles bundled for offline examples and regression checks. They are
test fixtures, not calibrated models or recommended policies. Functions that
need an accounting judgement require the caller to provide it explicitly.

The complete workflow returns an `fae_analysis_result` with status, metrics,
30 named output tables, 28 controls, parameters, and lineage. Accessors return
copies so callers cannot silently mutate the result object.

## Reproducible verification

From a source checkout with the packages in `Suggests` installed:

```r
roxygen2::roxygenise()
testthat::test_local()
source("tools/validate_api.R")
source("tools/verify_reference_parity.R")
```

`R CMD build .` followed by `R CMD check --as-cran` checks the exact source
archive. `tools/verify_release.R` additionally installs that archive into a
fresh library, runs installed-package and domain smoke tests, verifies both
profiles against the frozen neutral Golden Master, builds the standalone PDF
manual, and records SHA-256 evidence.

The repository includes the checked CRAN-style reference manual at
`docs/financialAccountingEngine-reference.pdf`. It is generated from the same
Rd sources that power `?` and `??`; it is never maintained by hand.

## Accounting and legal scope

The implementation is based on financial-instrument accounting requirements,
including IFRS 9 and related IFRS 7 disclosure concepts. IFRS is a descriptive
reference to the accounting basis and is not the product name. This project is
independent and is not affiliated with or endorsed by the IFRS Foundation or
the International Accounting Standards Board. It is reference software, not
audited accounting software, an accounting opinion or a substitute for an
entity's documented policies, judgements, model governance and approvals.

See [DISCLAIMER.md](DISCLAIMER.md) for the complete scope statement and
[THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md) for dependency and source-text
notices.

The code, documentation and bundled synthetic data are licensed under the
[Apache License 2.0](LICENSE.md). Copyright is held by RiskDataScience GmbH.
See the canonical [imprint](https://riskdatascience.net/impressum/) and
[privacy notice](https://riskdatascience.net/datenschutzerklaerung/).
