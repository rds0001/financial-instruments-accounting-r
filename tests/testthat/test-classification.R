.classification_fixture <- function() {
  data <- load_reference_data("MID_SIZE_UNIVERSAL_FIA")
  list(instrument=data$instruments[[1L]], features=data$classification_inputs[[1L]],
       stage=data$staging_inputs[[1L]], market=data$market_data[[1L]])
}

test_that("assess_scope returns the approved scope decision", {
  x <- .classification_fixture()
  expect_equal(assess_scope(x$instrument)$scope_decision, "IN_IFRS9_SCOPE")
})
test_that("assess_recognition exposes recognition facts", {
  x <- .classification_fixture()
  expect_equal(assess_recognition(x$instrument)$recognition_event, "RECOGNISED")
})
test_that("assess_business_model validates explicit judgements", {
  expect_true(assess_business_model("HOLD_TO_COLLECT")$judgement_required)
  expect_error(assess_business_model("UNKNOWN"), class="fae_accounting_error")
})
test_that("assess_sppi evaluates complete contractual features", {
  expect_true(assess_sppi(.classification_fixture()$features)$sppi_pass)
})
test_that("classify_instrument matches the reference asset", {
  x <- .classification_fixture()
  expect_equal(classify_instrument(x$instrument, x$features)$classification, "AMORTISED_COST")
})
test_that("assess_reclassification reports no unchanged business model", {
  x <- .classification_fixture()
  expect_equal(assess_reclassification(x$instrument, x$features)$reclassification_status,
               "NO_RECLASSIFICATION")
})
test_that("initial_measurement includes eligible transaction costs", {
  x <- initial_measurement(list(instrument_id="x", fair_value=100,
                                transaction_costs=2, transaction_price=100), "AMORTISED_COST")
  expect_equal(x$initial_carrying_amount, 102)
})
test_that("present_fair_value routes equity FVOCI movement to OCI", {
  x <- .classification_fixture()
  market <- x$market; market$total_fv_change <- 10
  expect_equal(present_fair_value(x$instrument, market, "FVOCI_EQUITY")$oci_fv_change, 10)
})
test_that("split_own_credit separates liability movements", {
  x <- split_own_credit(list(instrument_id="x", total_fv_change=10, own_credit_change=4),
                        "FVTPL_LIABILITY")
  expect_equal(x$other_fv_change_pnl, 6)
})
test_that("interest_revenue uses a net stage-three basis", {
  x <- interest_revenue(list(instrument_id="x", gross_amount=100, eir=0.1), "STAGE_3", 20)
  expect_equal(x$interest_revenue, 8)
})
test_that("impairment_scope identifies the general approach", {
  expect_equal(impairment_scope(.classification_fixture()$instrument)$approach, "GENERAL")
})
test_that("assess_stage preserves a clean stage-one case", {
  expect_equal(assess_stage(.classification_fixture()$stage)$stage, "STAGE_1")
})
test_that("assess_sicr responds to a watchlist trigger", {
  x <- .classification_fixture()$stage; x$watchlist <- TRUE
  expect_true(assess_sicr(x)$sicr)
})
test_that("stage_movements counts observed transitions", {
  x <- stage_movements(rep(list(list(prior_stage="STAGE_1", stage="STAGE_2")), 2L))
  expect_equal(x[[1L]]$instruments, 2L)
})
test_that("assess_ibor_relief records a controlled interface", {
  expect_equal(assess_ibor_relief(FALSE, "POLICY", "APPROVAL")$status,
               "CONTROLLED_INTERFACE")
})
