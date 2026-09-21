.workflow_result <- local({value<-NULL;function(){if(is.null(value))
  value<<-run_dataset(load_reference_data("SMALL_SA_RETAIL_FIA"));value}})

test_that("run_dataset calculates the complete small reference profile", {
  x <- .workflow_result(); expect_s3_class(x,"fae_analysis_result")
  expect_equal(x$status,"APPROVED_REFERENCE")
  expect_equal(x$metrics$opening_gross_exposure,590000)
  expect_length(x$tables,30L)
})
test_that("get_metrics returns an independent copy", {
  x <- get_metrics(.workflow_result());x$opening_gross_exposure<-0
  expect_equal(get_metrics(.workflow_result())$opening_gross_exposure,590000)
})
test_that("get_table resolves a unique sheet name", {
  expect_length(get_table(.workflow_result(),"STAGING"),5L)
})
test_that("get_controls returns 28 passed controls", {
  x <- get_controls(.workflow_result());expect_length(x,28L)
  expect_true(all(vapply(x,function(y)y$status=="PASS",logical(1))))
})
test_that("get_lineage records the public contract version", {
  expect_equal(get_lineage(.workflow_result())$contract_version,"1.1.0")
})
test_that("export_results writes seven grouped JSON files and manifests", {
  target<-tempfile("fae-results-");on.exit(unlink(target,recursive=TRUE),add=TRUE)
  export_results(.workflow_result(),target)
  expect_length(list.files(target,pattern="^[0-9]{2}_.*\\.json$"),7L)
  expect_true(all(file.exists(file.path(target,c("results.json","run_manifest.json")))))
})
