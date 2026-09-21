test_that("get_schema exposes all canonical tables and columns", {
  expect_length(get_schema(), 19L)
  expect_true("instrument_id" %in% unlist(get_schema("instruments")$columns))
  expect_error(get_schema("missing"), class = "fae_resource_error")
})

test_that("get_policy returns an explicit versioned independent policy", {
  x <- get_policy()
  expect_equal(x$past_due_sicr_days, 30)
  x$past_due_sicr_days <- 1
  expect_equal(get_policy()$past_due_sicr_days, 30)
})

test_that("get_parameters returns policy values with stable IDs", {
  expect_equal(get_parameters()$default_backstop_days, 90)
})

test_that("get_formula resolves a versioned registry row", {
  expect_equal(get_formula("F-ECL-001")$name, "component_ecl")
  expect_error(get_formula("UNKNOWN"), class = "fae_resource_error")
})

test_that("get_sources returns metadata without redistributed standard texts", {
  x <- get_sources()
  expect_length(x, 12L)
  expect_true(all(!vapply(x, function(y) y$redistributed, logical(1))))
})

test_that("list_profiles returns two stable identifiers", {
  expect_identical(list_profiles(), c("MID_SIZE_UNIVERSAL_FIA", "SMALL_SA_RETAIL_FIA"))
})

test_that("load_reference_data loads complete synthetic profiles", {
  x <- load_reference_data("SMALL_SA_RETAIL_FIA")
  expect_length(x, 19L)
  expect_length(x$instruments, 5L)
})

test_that("select_snapshot applies valid and known cutoffs", {
  x <- load_reference_data("SMALL_SA_RETAIL_FIA")
  y <- select_snapshot(list(instruments=x$instruments), "2026-12-31",
                       "2026-09-02T12:00:00+02:00")
  expect_length(y$instruments, 5L)
})

test_that("validate_dataset accepts the complete small profile", {
  x <- validate_dataset(load_reference_data("SMALL_SA_RETAIL_FIA"))
  expect_length(x, 19L)
  expect_length(x$instruments, 5L)
})

test_that("export_profile writes 19 canonical JSON tables and a manifest", {
  target <- tempfile("fae-export-")
  on.exit(unlink(target, recursive=TRUE), add=TRUE)
  result <- export_profile("SMALL_SA_RETAIL_FIA", target)
  expect_equal(normalizePath(result, winslash="/"), normalizePath(target, winslash="/"))
  expect_length(list.files(target, pattern="\\.json$"), 20L)
})
