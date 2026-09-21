<!-- Copyright 2026 RiskDataScience GmbH -->
<!-- SPDX-License-Identifier: Apache-2.0 -->

# Third-party notices

No third-party libraries or external accounting-standard texts are vendored in
this repository.

Runtime packages installed by users:

| R package | Constraint | Licence |
|---|---:|---|
| digest | any compatible CRAN release | GPL (>= 2) |
| jsonlite | any compatible CRAN release | MIT |

Development packages listed in `Suggests` are installed separately and remain
under their own licences: knitr (GPL), pkgbuild (MIT), pkgload (GPL-3),
rcmdcheck (MIT), rmarkdown (GPL-3), roxygen2 (MIT), and testthat (MIT).
Transitive dependencies are resolved by the user's R package installer and
remain governed by their own licences. Distributors must review the resolved
dependency inventory for their exact environment.

External accounting standards, legislation, supervisory publications, and
other source materials are not distributed. Their metadata and official
locations are bundled in `inst/extdata/sources.json` and exposed through
`get_sources()`; the repository licence does not apply to those materials.
