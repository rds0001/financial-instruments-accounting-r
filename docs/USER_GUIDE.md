# User guide

## What the package does

`financialAccountingEngine` is a native R reference engine for
financial-instrument accounting. Its public API supports both small,
independent calculations and a controlled full-profile run. It does not call
Python, depend on this repository at runtime, connect to a ledger, or download
accounting standards.

The public API has four layers:

1. Formula functions calculate discount factors, effective interest,
   amortised cost, probability curves, ECL components, cash shortfalls,
   recoveries, modifications, hedge ineffectiveness, and coverage.
2. Decision functions apply explicit facts and judgements to classification,
   staging, modifications, derecognition, credit enhancements, overlays, and
   hedge accounting.
3. Reporting functions create reference journals, rollforwards, disclosures,
   reconciliations, sensitivities, regulatory bridges, and model-monitoring
   results.
4. Resource and workflow functions validate canonical data, run the complete
   calculation, and expose metrics, tables, controls, and lineage.

## Units and input conventions

Rates, probabilities, and loss rates use decimal notation: five percent is
`0.05`. Time is measured in years. Monetary values passed to one function use
one currency unless an explicit FX mapping is part of that function's input.
Formula functions do not round money. Journal and reporting consumers must
apply their approved currency and posting policy at the appropriate boundary.

Inputs are ordinary named R lists, vectors, matrices, and data frames. Missing
required facts, non-finite numbers, inconsistent keys, invalid probability
weights, and absent approval references raise typed errors. The engine does not
invent a business model, override, hedge designation, model approval, or
institutional accounting conclusion.

## A granular calculation

```r
library(financialAccountingEngine)

component_ecl(
  marginal_pd = 0.02,
  ead = 100000,
  lgd = 0.40,
  eir = 0.05,
  year = 1
)
```

The result is the discounted PD × EAD × LGD component for the supplied default
time. `?component_ecl` gives the units, accepted ranges, return type, formula
context, and related scenario workflow.

## A decision with explicit facts

```r
facts <- list(
  instrument_id = "loan-1",
  impairment_scope = "GENERAL_ECL",
  days_past_due = 35,
  rating_orig = 3,
  rating_current = 5
)

assess_stage(facts)
```

The packaged policy is fixed and versioned. Pass a policy list explicitly when
the analysis must use a different approved policy. A default selects the
documented packaged version; it does not fill missing transaction or model
facts.

## Complete offline reference run

```r
data <- load_reference_data("SMALL_SA_RETAIL_FIA")
result <- run_dataset(data)

result$status
get_metrics(result)
names(result$tables)
get_controls(result)
get_lineage(result)
```

The result contains 30 named output tables and 28 controls. Its accessors return
copies. `APPROVED_REFERENCE` means all package controls passed for the supplied
data. It is not an audit opinion, posting approval, model approval, or entity
accounting conclusion.

Use `get_table(result, "ECL_RESULTS")` for instrument ECL,
`get_table(result, "DISCLOSURE_FACTS")` for disclosure facts, and
`get_table(result, "JOURNALS")` for balanced reference entries. Journal account
mapping and actual posting remain institutional responsibilities.

## Synthetic profiles and reproducibility

`SMALL_SA_RETAIL_FIA` and `MID_SIZE_UNIVERSAL_FIA` are synthetic, deterministic
fixtures. They contain all 19 canonical input tables and can be used offline in
tests, examples, and demonstrations. They contain no real customer, bank, or
personal data and are not recommended policy or calibration data.

`get_lineage()` identifies the input, policy, rule-set, code, and resource
hashes used for a run. `export_results()` writes results only to a caller-chosen
new directory. It does not mutate packaged resources.

## Finding the right help

```r
?financialAccountingEngine
??financialAccountingEngine
help(package = "financialAccountingEngine")
vignette(package = "financialAccountingEngine")
```

The package overview links the six subject-level reference pages. Searching a
function name resolves its indexed alias even when related functions share one
page. `tools/api_inventory.csv` maps every export to its signature, Rd page,
direct test, and example.

## Professional scope

The package is an independent reference implementation based on
financial-instrument accounting requirements, including IFRS 9 and related
IFRS 7 disclosure concepts. Those names identify the accounting basis; they
are not the product name. The project is not affiliated with or endorsed by
the IFRS Foundation or the International Accounting Standards Board.

The software does not replace authoritative standards, applicable law,
professional advice, entity policies and judgements, model validation, or
financial-close controls. See `DISCLAIMER.md` and `THIRD_PARTY_NOTICES.md` in
the repository for the complete legal and dependency context.
