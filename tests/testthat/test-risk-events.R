.risk_fixture <- function() {
  list(instrument=list(instrument_id="x", impairment_scope="GENERAL_ECL",
    ecl_method="COMPONENT_PD_LGD_EAD", eir=0.05, overlay=0),
    pd=list(list(scenario_id="base",year=1,marginal_pd=0.02)),
    lgd=list(list(scenario_id="base",year=1,lgd=0.4)),
    ead=list(list(scenario_id="base",year=1,ead=100000)))
}
.event <- function(type, ...) c(list(event_id="E",instrument_id="x",event_type=type,
  approved=TRUE,reason_code="APPROVED",amount=10),list(...))
.source <- list(instrument_id="x",gross_amount=100)

test_that("allocate_credit_enhancements respects capacity", {
  x <- allocate_credit_enhancements(list(list(collateral_id="C",instrument_id="x",
    fair_value=100,haircut=.2,cost=5,allocated_amount=70,recovery_year=2)),
    list(list(instrument_id="x")))
  expect_equal(x[[1]]$available_protection,75)
})
test_that("scenario_ecl matches the independent component case", {
  x <- .risk_fixture(); r <- scenario_ecl(x$instrument,"STAGE_1",x$pd,x$lgd,x$ead,c(base=1))
  expect_equal(r$summary$final_ecl,761.9047619047619,tolerance=1e-12)
})
test_that("validate_overlay enforces governance", {
  x <- list(overlay_id="O",overlay_owner="Risk",overlay_approved=TRUE,
    overlay_expiry="2026-12-31",overlay_thesis="Risk",overlay_double_count_checked=TRUE,overlay=10)
  expect_equal(validate_overlay(x,"2026-12-31")$status,"VALID")
})
test_that("allocate_overlay applies governed weights", {
  expect_equal(allocate_overlay(10,c(x=.3,y=.7)),list(x=3,y=7))
})
test_that("reconcile_overlay controls the total", {
  expect_equal(reconcile_overlay(10,c(x=3,y=7))$status,"PASS")
})
test_that("assess_derecognition processes a risk transfer", {
  expect_equal(assess_derecognition(.event("ASSET_TRANSFER",risks_rewards_transferred=TRUE),
    .source)$derecognition_result,"FULL_DERECOGNITION")
})
test_that("continuing_involvement remains bounded", {
  x <- .event("ASSET_TRANSFER",control_retained=TRUE,continuing_involvement_amount=4)
  expect_equal(continuing_involvement(x,.source)$continuing_involvement,4)
})
test_that("process_modification returns the signed asset effect", {
  x <- .event("ASSET_MODIFICATION_NO_DERECOGNITION",amount=0,
    modified_present_value=90,derecognition_approved=FALSE)
  expect_equal(process_modification(x,.source)$pnl_effect,-10)
})
test_that("process_writeoff validates the carrying amount", {
  expect_equal(process_writeoff(.event("WRITEOFF"),.source)$derecognition_result,"WRITEOFF")
})
test_that("process_recovery reports positive income", {
  expect_equal(process_recovery(.event("RECOVERY"),.source)$pnl_effect,10)
})
.hedge <- function() list(relationship_id="H",hedge_type="CASH_FLOW",hedging_change=120,
  hedged_change=-100,hedge_ratio=1,target_hedge_ratio=1,hedging_instrument_eligible=TRUE,
  hedged_item_eligible=TRUE,risk_component_eligible=TRUE,designation_documented=TRUE,
  risk_management_objective="Risk",designated=TRUE,economic_relationship=TRUE,
  credit_risk_dominates=FALSE,forecast_transaction_expected=TRUE)
test_that("measure_hedge applies the lower-of rule", {
  x <- measure_hedge(.hedge()); expect_equal(x$oci_effective,100); expect_equal(x$ineffectiveness_pnl,20)
})
test_that("validate_hedge_designation reports eligibility", {
  expect_true(validate_hedge_designation(.hedge())$eligible)
})
test_that("assess_hedge_effectiveness returns measurement", {
  expect_equal(assess_hedge_effectiveness(.hedge())$status,"ELIGIBLE")
})
test_that("oci_rollforward applies signed movements", {
  expect_equal(oci_rollforward(10,fair_value_movement=5,recycling=2,basis_adjustment=1)$closing_reserve,12)
})
test_that("hedge_reserve_rollforward aggregates result rows", {
  h <- list(oci_effective=5,cost_of_hedging_reserve=3,recycle_amount=2,basis_adjustment=1)
  expect_equal(hedge_reserve_rollforward(10,list(h))$closing_reserve,15)
})
