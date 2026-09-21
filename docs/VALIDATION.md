# Validation and release evidence

Validation is layered so a passing development test cannot stand in for an
installable, documented package.

## Direct API tests

Every one of the 80 exports has a direct `testthat` case. Tests cover expected
calculations, rejected inputs, decision precedence, rollforward balance,
journal balance, copy semantics, and end-to-end controls. `R CMD check` also
runs all examples and rebuilds every vignette.

## Documentation inventory

`tools/api_inventory.csv` records each exported function, signature, Rd alias,
Rd page, direct test, and example identifier. `tools/validate_api.R` compares
that inventory with the development or installed namespace and Rd database. It
fails on missing or duplicate aliases, signature drift, absent mandatory Rd
sections, missing function calls in examples, or missing direct tests.

## Independent reference parity

`tools/verify_reference_parity.R` runs both complete synthetic profiles and
compares status, metrics, and every table with the frozen neutral Golden
Master. Numeric comparisons use explicit absolute and relative tolerances.
Technical fields such as run timestamps may be excluded only through the
reviewed Golden-Master manifest. The R calculation is native; no Python process
participates in this check.

## Distribution verification

`tools/verify_release.R` performs the release gate:

1. inspect the source boundary and reject placeholders or scaffold content;
2. build a source tarball with vignettes and verify its members;
3. run `R CMD check --as-cran` on that exact tarball;
4. install it into a fresh temporary R library;
5. validate the installed namespace, help database, API, domain run, controls,
   and rejected-input path from outside the source directory;
6. compare both installed-profile results with the Golden Master;
7. build the standalone CRAN-style PDF manual from the unpacked tarball; and
8. record the tarball, SHA-256, check result, parity report, PDF, and session
   information under `release-evidence/`.

Errors and warnings always fail. Notes fail unless they match the narrowly
defined new-submission case. A developer may explicitly allow only DNS/time
NOTEs when running inside an offline isolated environment; the online GitHub
matrix repeats the full check on Linux, Windows, and macOS with R oldrel,
release, and devel.
