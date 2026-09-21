## Test environments

- Local: R 4.1.2, x86_64-pc-linux-gnu, Debian/WSL, isolated network
- GitHub Actions: R oldrel, release, and devel on Linux; R release on Windows
  and macOS (pending repository publication)

## R CMD check results

Local exact source-tarball check: 0 errors, 0 warnings, 2 notes.

Both local notes are caused by the isolated runner: it cannot resolve CRAN,
GitHub, riskdatascience.net, or ifrs.org, and it cannot reach an external time
source. The same run completed URL-independent incoming checks, installation,
code analysis, all examples, all tests, all vignettes, and the PDF manual.
Online multi-platform results will replace this provisional record before any
CRAN submission.

This is a new submission. A reverse-dependency population has not yet been
queried because the package has not been submitted to CRAN.

## Scope and data

This is a new native R package. It performs no network access and bundles only
fully synthetic offline reference profiles. It does not redistribute accounting
standards or legal texts.
