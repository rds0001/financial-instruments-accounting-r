.report_fixture <- function() list(
  instruments=list(list(instrument_id="x",gross_amount=100,entity_id="E",portfolio="P",
    currency="EUR",regulatory_expected_loss=15,regulatory_approach="STANDARDISED")),
  ecl=list(list(instrument_id="x",stage="STAGE_1",final_ecl=20)))
.obs <- function(type="PD") list(model_type=type,model_version_id="v1",predicted_value=.2,
  actual_value=.1,exposure_weight=1,outcome_flag=FALSE,model_owner="Risk",
  validation_status="VALIDATED",approval_id="A",segment="X")

test_that("build_journals creates a balanced allowance entry", {
  x <- .report_fixture(); r <- build_journals(x$ecl,c(x=5),c(x="AMORTISED_COST"),x$instruments)
  expect_length(r,2L); expect_equal(sum(vapply(r,function(y)y$debit-y$credit,numeric(1))),0)
})
test_that("allowance_rollforward bridges opening to closing", {
  x <- .report_fixture(); r <- allowance_rollforward(x$instruments,c(x=5),x$ecl,list(),c(x=1))
  expect_equal(r[[1]]$closing_allowance,20); expect_equal(r[[1]]$ecl_remeasurement,15)
})
test_that("gross_rollforward reports the contractual residual", {
  x <- .report_fixture(); r <- gross_rollforward(x$instruments,c(x=90),list(),c(x=1))
  expect_equal(r[[1]]$closing_gross,100); expect_equal(r[[1]]$other_contractual_and_fx_movements,10)
})
test_that("reconcile_ledger reports a failed difference", {
  expect_equal(reconcile_ledger(100,99)$status,"FAIL")
})
test_that("disclosure_facts creates atomic net amounts", {
  x <- .report_fixture(); r <- disclosure_facts(x$instruments,x$ecl,c(x="AMORTISED_COST"),
    c(x="STAGE_1"),c(x=1),"EUR"); expect_equal(r[[1]]$net_carrying_amount,80)
})
test_that("aggregate_disclosures computes six dimensional views", {
  x <- .report_fixture(); f <- disclosure_facts(x$instruments,x$ecl,c(x="AMORTISED_COST"),
    c(x="STAGE_1"),c(x=1),"EUR"); r <- aggregate_disclosures(f)
  expect_length(r,6L); expect_true(all(vapply(r,function(y)y$coverage_ratio==.2,logical(1))))
})
test_that("backtest_pd evaluates PD observations", {
  expect_equal(backtest_pd(list(.obs()))[[1]]$mean_absolute_error,.1)
})
test_that("backtest_lgd evaluates LGD observations", {
  expect_equal(backtest_lgd(list(.obs("LGD")))[[1]]$calibration_ratio,.5)
})
test_that("backtest_ead evaluates EAD observations", {
  expect_equal(backtest_ead(list(.obs("EAD")))[[1]]$bias,.1)
})
test_that("backtest_staging evaluates staging observations", {
  expect_equal(backtest_staging(list(.obs("STAGING")))[[1]]$observations,1L)
})
test_that("backtest_models accepts governed CCF monitoring", {
  expect_equal(backtest_models(list(.obs("CCF")))[[1]]$mean_absolute_error,.1)
})
test_that("scenario_sensitivity returns weighted contributions", {
  r <- scenario_sensitivity(c(base=10,stress=30),c(base=.75,stress=.25))
  expect_equal(sum(vapply(r,function(x)x$weighted_contribution,numeric(1))),15)
})
test_that("compare_challenger reports metric differences", {
  r <- compare_challenger(list(list(model_type="PD",segment="X",mean_absolute_error=.1,bias=.02)),
    list(list(model_type="PD",segment="X",mean_absolute_error=.05,bias=.01)))
  expect_equal(r[[1]]$mae_difference,-.05)
})
test_that("regulatory_bridge remains separate from accounting ECL", {
  x <- .report_fixture(); r <- regulatory_bridge(x$instruments,x$ecl,c(x="STAGE_1"),c(x=1))
  expect_equal(r[[1]]$difference,5); expect_false(r[[1]]$accounting_value_used_in_regulatory_calculation)
})
