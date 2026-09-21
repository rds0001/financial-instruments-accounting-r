# Contributing

Contributions must preserve the public API contract, reject incomplete
accounting inputs, and include independent evidence for every change to a
financial result.

1. Explain the accounting or engineering reason for the change.
2. Add a direct test for each affected public function. Do not calculate the
   expected value with the implementation under test.
3. Regenerate documentation with `roxygen2::roxygenise()` and verify that
   `man/`, `NAMESPACE`, and `DESCRIPTION` contain no unexplained drift.
4. Run `testthat::test_local()`, `tools/validate_api.R`,
   `tools/verify_reference_parity.R`, and the source-tarball checks described
   in the README.
5. Update the relevant manpage, vignette, inventory entry, and `NEWS.md` when
   public behaviour changes.

Never commit client data, credentials, licensed standard text, or output
labelled as an official accounting conclusion. By contributing, you agree that
your contribution is licensed under Apache License 2.0.
