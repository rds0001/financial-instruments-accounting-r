test_that("discount_factor computes annual compound discounting", {
  expect_equal(discount_factor(0.05, 1), 1 / 1.05, tolerance = 1e-12)
  expect_error(discount_factor(-1, 1), class = "fae_accounting_error")
})

test_that("effective_interest_rate solves the independent golden case", {
  expect_equal(effective_interest_rate(1000, list(c(1, 1100))), 0.1, tolerance = 1e-10)
})

test_that("amortised_cost_step applies effective interest", {
  expect_equal(amortised_cost_step(1000, 0.1, 1, 200), 900)
})

test_that("amortisation_schedule preserves period detail", {
  x <- amortisation_schedule(1000, 0.1, list(c(1, 200), c(2, 990)))
  expect_equal(x$interest, c(100, 90))
  expect_equal(x$closing, c(900, 0))
})

test_that("marginal_pd_from_cumulative differences a valid curve", {
  expect_equal(marginal_pd_from_cumulative(c(0.1, 0.3)), c(0.1, 0.2))
  expect_error(marginal_pd_from_cumulative(c(0.2, 0.1)), class = "fae_accounting_error")
})

test_that("survival_from_pd computes end-period survival", {
  expect_equal(survival_from_pd(c(0.1, 0.2)), c(0.9, 0.7))
})

test_that("cumulative_pd_from_conditional applies survival logic", {
  expect_equal(cumulative_pd_from_conditional(c(0.1, 0.2)), c(0.1, 0.28))
})

test_that("component_ecl matches the independent golden case", {
  expect_equal(component_ecl(0.02, 100000, 0.4, 0.05, 1),
               761.9047619047619, tolerance = 1e-12)
})

test_that("cash_shortfall_ecl discounts both legs", {
  expect_equal(cash_shortfall_ecl(list(c(1, 105)), list(c(2, 55.125)), 0.05), 50)
})

test_that("modification_gain_loss returns a signed asset effect", {
  expect_equal(modification_gain_loss(100, list(c(1, 99)), 0.1), -10)
})

test_that("hedge_ineffectiveness returns the signed residual", {
  expect_equal(hedge_ineffectiveness(100, -90), 10)
})

test_that("ead_profile combines drawn and converted undrawn amounts", {
  expect_equal(ead_profile(100, 50, 0.4, 0.1), 110)
})

test_that("recovery_present_value discounts recovery timing", {
  expect_equal(recovery_present_value(list(c(2, 121)), 0.1), 100, tolerance = 1e-12)
})

test_that("lgd_from_recoveries matches the independent golden case", {
  expect_equal(lgd_from_recoveries(100, list(c(1, 55)), 0.1), 0.5, tolerance = 1e-12)
})

test_that("provision_matrix_ecl multiplies exposure and loss rate", {
  expect_equal(provision_matrix_ecl(1000, 0.03), 30)
})

test_that("weighted_ecl weights separately calculated scenarios", {
  expect_equal(weighted_ecl(c(base=10, stress=30), c(base=0.75, stress=0.25)), 15)
  expect_error(weighted_ecl(c(base=10), c(base=0.9)), class = "fae_accounting_error")
})

test_that("poci_allowance_change remains signed", {
  expect_equal(poci_allowance_change(20, 30), -10)
})

test_that("liability_modification_test includes threshold equality", {
  x <- liability_modification_test(100, 110)
  expect_equal(x$ratio, 0.1)
  expect_true(x$substantial)
})

test_that("coverage_ratio handles a zero denominator explicitly", {
  expect_equal(coverage_ratio(20, 100), 0.2)
  expect_true(is.na(coverage_ratio(0, 0)))
})

test_that("benchmark_ecl returns the fixed checksum", {
  x <- benchmark_ecl(10)
  expect_equal(x$checksum, 7619.047619047619, tolerance = 1e-10)
  expect_equal(x$formula_id, "F-ECL-001")
})
