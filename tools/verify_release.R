# Verify an exact R source-package release candidate end to end.

main <- function() {
  root <- normalizePath(Sys.getenv("FAE_SOURCE_ROOT", "."), winslash = "/",
                        mustWork = TRUE)
  description <- read.dcf(file.path(root, "DESCRIPTION"))
  package <- description[1L, "Package"]
  version <- description[1L, "Version"]
  stopifnot(package == "financialAccountingEngine",
            description[1L, "License"] == "Apache License (== 2.0)")

  required <- c(
    "LICENSE.md", "inst/NOTICE", "docs/financialAccountingEngine-reference.pdf",
    "tools/api_inventory.csv",
    "tools/validate_api.R", "tools/smoke_installed.R",
    "tools/smoke_domain.R", "tools/verify_reference_parity.R"
  )
  stopifnot(all(file.exists(file.path(root, required))))
  stopifnot(file.info(file.path(root,
    "docs/financialAccountingEngine-reference.pdf"))$size > 0L)

  source_files <- list.files(root, all.files = TRUE, recursive = TRUE,
                             no.. = TRUE)
  source_files <- source_files[!grepl(
    "^(\\.git|\\.work|release-evidence|dist|build)/|[.]Rcheck/|[.]tar[.]gz$",
    source_files
  )]
  stopifnot(!any(grepl(
    "(^|/)(scaffold[^/]*|downloads|downloaded-documents|[.]venv|__pycache__)(/|$)",
    source_files, ignore.case = TRUE
  )))
  text_files <- source_files[grepl("[.](R|Rd|md|Rmd|json|ya?ml|csv|cff)$",
                                   source_files, ignore.case = TRUE)]
  for (name in text_files) {
    lines <- readLines(file.path(root, name), warn = FALSE)
    stopifnot(!any(grepl("__[A-Z][A-Z0-9_]*__", lines)))
  }

  work <- tempfile("fae-release-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  artifact <- pkgbuild::build(root, dest_path = work, vignettes = TRUE,
                              manual = FALSE)
  members <- utils::untar(artifact, list = TRUE)
  forbidden <- "(^|/)(scaffold[^/]*|downloads|downloaded-documents|[.]git|[.]cache|[.]venv|__pycache__|[.]work)(/|$)"
  stopifnot(!any(grepl(forbidden, members, ignore.case = TRUE)),
            !any(grepl("(^/|(^|/)[.][.](/|$))", members)),
            !any(startsWith(members, paste0(package, "/docs/"))),
            !paste(package, "LICENSE.md", sep = "/") %in% members)
  required_members <- c(
    "DESCRIPTION", "NAMESPACE", "inst/NOTICE", "build/vignette.rds",
    "tools/api_inventory.csv", "tools/validate_api.R",
    "tools/smoke_installed.R", "tools/smoke_domain.R",
    "tools/verify_reference_parity.R"
  )
  stopifnot(all(file.path(package, required_members) %in% members))

  check_dir <- file.path(work, "check")
  dir.create(check_dir)
  result <- rcmdcheck::rcmdcheck(
    artifact, args = "--as-cran", error_on = "never", check_dir = check_dir
  )
  stopifnot(length(result$errors) == 0L, length(result$warnings) == 0L)

  note_text <- vapply(result$notes, paste, "", collapse = "\n")
  is_new_submission <- function(note) {
    grepl("New submission", note, fixed = TRUE) &&
      !grepl("possibly.*invalid|unable to|ERROR|WARNING", note,
             ignore.case = TRUE)
  }
  allow_offline <- identical(tolower(Sys.getenv("FAE_ALLOW_OFFLINE_CHECK_NOTES")),
                             "true")
  allow_transient_urls <- identical(
    tolower(Sys.getenv("FAE_ALLOW_TRANSIENT_URL_NOTES")), "true"
  )
  is_offline_infrastructure <- function(note) {
    time_only <- grepl("unable to verify current time", note, fixed = TRUE)
    dns_only <- grepl("Could not resolve host|Couldn't resolve host name", note) &&
      !grepl("Status: (?!Error)", note, perl = TRUE) &&
      !grepl("libcurl error code (?!6)", note, perl = TRUE)
    time_only || dns_only
  }
  is_known_transient_url_failure <- function(note) {
    lines <- strsplit(note, "\n", fixed = TRUE)[[1L]]
    url_lines <- grep("^[[:space:]]*URL:[[:space:]]*", lines, value = TRUE)
    urls <- sub("^[[:space:]]*URL:[[:space:]]*", "", url_lines)
    allowed_urls <- c(
      "https://riskdatascience.net/impressum/",
      "https://riskdatascience.net/datenschutzerklaerung/"
    )
    length(urls) > 0L && all(urls %in% allowed_urls) &&
      grepl("Status: 50[0-4]", note)
  }
  accepted <- vapply(note_text, is_new_submission, logical(1)) |
    (allow_offline & vapply(note_text, is_offline_infrastructure, logical(1))) |
    (allow_transient_urls &
       vapply(note_text, is_known_transient_url_failure, logical(1)))
  if (any(!accepted)) {
    stop("Unapproved R CMD check NOTE(s):\n", paste(note_text[!accepted], collapse = "\n---\n"))
  }

  lib <- file.path(work, "library")
  dir.create(lib)
  suffix <- if (.Platform$OS.type == "windows") ".exe" else ""
  r <- file.path(R.home("bin"), paste0("R", suffix))
  rscript <- file.path(R.home("bin"), paste0("Rscript", suffix))
  run <- function(command, args, wd = root) {
    old <- setwd(wd)
    on.exit(setwd(old))
    status <- system2(command, args = vapply(args, shQuote, character(1)))
    stopifnot(identical(status, 0L))
  }
  run(r, c("CMD", "INSTALL", paste0("--library=", lib), artifact))
  run(rscript, c("--vanilla", file.path(root, "tools/validate_api.R"),
                 package, root, lib))

  smoke <- file.path(work, "outside-source-smoke")
  dir.create(smoke)
  smoke_scripts <- c("tools/smoke_installed.R", "tools/smoke_domain.R")
  stopifnot(all(file.copy(file.path(root, smoke_scripts), smoke)))
  run(rscript, c("--vanilla", "smoke_installed.R", package, lib), smoke)
  run(rscript, c("--vanilla", "smoke_domain.R", package, lib), smoke)

  evidence <- file.path(root, "release-evidence")
  dir.create(evidence, showWarnings = FALSE)
  run(rscript, c("--vanilla", file.path(root, "tools/verify_reference_parity.R"),
                 package, lib, evidence))

  unpacked <- file.path(work, "manual-source")
  dir.create(unpacked)
  utils::untar(artifact, exdir = unpacked)
  manual_name <- paste0(package, "_", version, "-manual.pdf")
  manual <- file.path(work, manual_name)
  run(r, c("CMD", "Rd2pdf", "--no-preview", paste0("--output=", manual),
           file.path(unpacked, package)))
  stopifnot(file.exists(manual), file.info(manual)$size > 0L)

  stopifnot(file.copy(manual, file.path(evidence, manual_name), overwrite = TRUE))
  stopifnot(file.copy(artifact, evidence, overwrite = TRUE))
  saveRDS(result, file.path(evidence, "check-results.rds"))
  writeLines(capture.output(sessionInfo()), file.path(evidence, "session-info.txt"))
  sha256 <- digest::digest(file = artifact, algo = "sha256")
  writeLines(paste(sha256, basename(artifact)),
             file.path(evidence, "tarball.sha256"))
  writeLines(c(
    paste0("Package: ", package, " ", version),
    paste0("Errors: ", length(result$errors)),
    paste0("Warnings: ", length(result$warnings)),
    paste0("Notes: ", length(result$notes)),
    if (length(note_text)) paste0("Accepted note: ", note_text) else "Notes: none"
  ), file.path(evidence, "check-summary.txt"))
  cat("R release candidate checks: PASS\n")
}

main()
