# Documentation map

The installed package is the primary documentation system:

```r
?financialAccountingEngine
??financialAccountingEngine
help(package = "financialAccountingEngine")
vignette(package = "financialAccountingEngine")
```

The package overview explains the scope and entry points. Each of the 80
exports has an indexed Rd alias, its real signature, all arguments and return
values, accounting context, references, related functions, and a runnable
offline example. The four vignettes connect those functions into common tasks.

- [USER_GUIDE.md](USER_GUIDE.md) explains the package's operating model and
  how to interpret a full result.
- [VALIDATION.md](VALIDATION.md) explains the test, parity, build, and release
  evidence.
- [financialAccountingEngine-reference.pdf](financialAccountingEngine-reference.pdf)
  is the CRAN-style PDF reference manual generated from `man/*.Rd`.

Regenerate documentation and the reference manual from the repository root:

```r
roxygen2::roxygenise()
source("tools/validate_api.R")
```

```sh
R CMD Rd2pdf --no-preview \
  --output=docs/financialAccountingEngine-reference.pdf .
```

Generated Rd files and the PDF must describe the same package version. The
release workflow rebuilds a second PDF from the exact source tarball and stores
it with the release evidence.
