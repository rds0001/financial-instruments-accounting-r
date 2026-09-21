#' Core Financial-Instrument Accounting Formulas
#'
#' Pure deterministic calculations for discounting, effective interest,
#' amortised cost, probability curves, credit loss, recoveries, modifications,
#' hedges and coverage. Rates and probabilities are decimals; time is measured
#' in years; monetary values within one call use one currency.
#'
#' @param rate,eir,original_eir Annual effective rate as a decimal, strictly greater than
#'   `-1` where discounting is performed.
#' @param year_fraction,time,year Non-negative time in years.
#' @param initial_net Positive initial net carrying amount.
#' @param cashflows,contractual,expected,modified_cashflows,recoveries,costs
#'   Two-column matrix/data frame or list of `(time, amount)` pairs. Times and
#'   amounts are finite and non-negative.
#' @param tolerance Positive numerical tolerance. For
#'   `liability_modification_test()`, the policy threshold in `[0, 1]`.
#' @param opening,gross_before,ead,exposure,allowance,gross_exposure Monetary
#'   amounts in the call's currency. Required non-negative unless the function
#'   expressly returns a signed result.
#' @param cash_received Non-negative receipt in the period.
#' @param adjustment Signed period adjustment.
#' @param cumulative Non-decreasing cumulative default probabilities in `[0,1]`.
#' @param marginal Unconditional marginal default probabilities in `[0,1]`.
#' @param conditional Conditional period default probabilities in `[0,1]`.
#' @param marginal_pd,lgd,loss_rate Probabilities or rates in `[0,1]`.
#' @param hedging_change,hedged_change Signed fair-value changes; gains are
#'   positive.
#' @param drawn,undrawn Non-negative exposure vectors.
#' @param ccf,prepayment Equal-length rate vectors in `[0,1]`.
#' @param scenario_losses Named non-negative scenario losses.
#' @param probabilities Named scenario probabilities whose keys match losses
#'   and whose sum is one within `1e-9`.
#' @param current_lifetime_ecl,initial_lifetime_ecl Non-negative lifetime loss
#'   estimates; the returned POCI movement remains signed.
#' @param original_present_value,modified_present_value Non-negative present
#'   values measured using the original effective interest rate.
#' @param qualitative_substantial One non-missing logical management judgement.
#' @param iterations Integer count from 1 through 1,000,000.
#'
#' @return Scalar formula functions return one numeric value. Probability and
#'   EAD curve functions return numeric vectors in input order.
#'   `amortisation_schedule()` returns a data frame with `time`, `opening`,
#'   `interest`, `cash_received`, and `closing`. The liability modification
#'   test returns a named list with `ratio`, `substantial`, and `threshold`.
#'   `coverage_ratio()` returns `NA_real_` for a zero denominator. The benchmark
#'   returns iterations, checksum, elapsed seconds, peak bytes (unavailable and
#'   therefore `NA_real_` in portable R), and formula identifier.
#'
#' @details No function rounds monetary results. Inputs must be finite and
#'   missing values are rejected. `effective_interest_rate()` uses a bracketed
#'   bisection root over `[-0.999999, 10]`, with at most 250 iterations.
#'   `component_ecl()` discounts PD times EAD times LGD to the supplied default
#'   time. Scenario losses must be calculated separately before
#'   `weighted_ecl()` is called. Policy judgements, model calibration and data
#'   approval remain caller responsibilities.
#'
#' @references International Accounting Standards Board (2014), IFRS 9,
#'   paragraphs 5.4.1, 5.5.17, B3.3.6 and Appendix A (effective interest and
#'   expected credit losses). See the independently maintained official source
#'   index at <https://www.ifrs.org/issued-standards/list-of-standards/ifrs-9-financial-instruments/>.
#'
#' @seealso [scenario_ecl()], [interest_revenue()], [process_modification()]
#'
#' @examples
#' discount_factor(0.05, 1)
#' stopifnot(abs(effective_interest_rate(1000, list(c(1, 1100))) - 0.1) < 1e-10)
#' amortised_cost_step(1000, 0.1, 1, 200)
#' amortisation_schedule(1000, 0.1, list(c(1, 200), c(2, 990)))
#' marginal_pd_from_cumulative(c(0.1, 0.3))
#' survival_from_pd(c(0.1, 0.2))
#' cumulative_pd_from_conditional(c(0.1, 0.2))
#' component_ecl(0.02, 100000, 0.4, 0.05, 1)
#' cash_shortfall_ecl(list(c(1, 105)), list(c(2, 55.125)), 0.05)
#' modification_gain_loss(100, list(c(1, 99)), 0.1)
#' hedge_ineffectiveness(100, -90)
#' ead_profile(100, 50, 0.4, 0.1)
#' recovery_present_value(list(c(2, 121)), 0.1)
#' lgd_from_recoveries(100, list(c(1, 55)), 0.1)
#' provision_matrix_ecl(1000, 0.03)
#' weighted_ecl(c(base = 10, stress = 30), c(base = 0.75, stress = 0.25))
#' poci_allowance_change(20, 30)
#' liability_modification_test(100, 110)
#' coverage_ratio(20, 100)
#' benchmark_ecl(10)
#' @name core_formulas
NULL

#' @rdname core_formulas
#' @export
discount_factor <- function(rate, year_fraction) {
  rate <- .fae_number(rate, "rate")
  year_fraction <- .fae_number(year_fraction, "year_fraction", 0)
  if (rate <= -1) .fae_abort("rate must exceed -100%")
  result <- (1 + rate)^(-year_fraction)
  if (!is.finite(result) || result < 0) .fae_abort("Discount factor overflow")
  result
}

#' @rdname core_formulas
#' @export
effective_interest_rate <- function(initial_net, cashflows, tolerance = 1e-12) {
  initial_net <- .fae_number(initial_net, "initial_net", 0)
  tolerance <- .fae_number(tolerance, "tolerance", 0, 1e-3)
  flows <- .fae_cashflows(cashflows, "cashflows", FALSE)
  if (initial_net == 0 || tolerance == 0 || !any(flows[, 1L] > 0 & flows[, 2L] > 0)) {
    .fae_abort("Positive initial amount, tolerance and future receipts required")
  }
  npv <- function(r) sum(flows[, 2L] * vapply(flows[, 1L], function(t) {
    discount_factor(r, t)
  }, numeric(1))) - initial_net
  lo <- -0.999999
  hi <- 10
  flo <- npv(lo)
  fhi <- npv(hi)
  if (flo * fhi > 0) .fae_abort("EIR root outside [-0.999999, 10]")
  for (i in seq_len(250L)) {
    mid <- (lo + hi) / 2
    value <- npv(mid)
    if (abs(value) <= tolerance * max(1, initial_net) || hi - lo <= tolerance) return(mid)
    if (flo * value <= 0) hi <- mid else {
      lo <- mid
      flo <- value
    }
  }
  .fae_abort("EIR failed to converge")
}

#' @rdname core_formulas
#' @export
amortised_cost_step <- function(opening, eir, time, cash_received, adjustment = 0) {
  opening <- .fae_number(opening, "opening", 0)
  eir <- .fae_number(eir, "eir")
  time <- .fae_number(time, "time", 0)
  cash_received <- .fae_number(cash_received, "cash_received", 0)
  adjustment <- .fae_number(adjustment, "adjustment")
  discount_factor(eir, 0)
  opening + opening * eir * time + adjustment - cash_received
}

#' @rdname core_formulas
#' @export
amortisation_schedule <- function(opening, eir, cashflows) {
  balance <- .fae_number(opening, "opening", 0)
  flows <- .fae_cashflows(cashflows, "cashflows")
  prior <- 0
  rows <- vector("list", nrow(flows))
  for (i in seq_len(nrow(flows))) {
    time <- flows[i, 1L]
    cash <- flows[i, 2L]
    if (time <= prior) .fae_abort("Times must increase strictly")
    interest <- balance * .fae_number(eir, "eir") * (time - prior)
    closing <- amortised_cost_step(balance, eir, time - prior, cash)
    if (closing < -1e-9) .fae_abort("Schedule overpayment")
    rows[[i]] <- data.frame(time = time, opening = balance, interest = interest,
                            cash_received = cash, closing = closing)
    prior <- time
    balance <- max(closing, 0)
  }
  if (!length(rows)) data.frame(time=numeric(), opening=numeric(), interest=numeric(),
                                cash_received=numeric(), closing=numeric())
  else do.call(rbind, rows)
}

#' @rdname core_formulas
#' @export
marginal_pd_from_cumulative <- function(cumulative) {
  cumulative <- .fae_vector(cumulative, "cumulative PD", 0, 1)
  if (length(cumulative) > 1L && any(diff(cumulative) < 0)) {
    .fae_abort("Cumulative PD must be non-decreasing")
  }
  diff(c(0, cumulative))
}

#' @rdname core_formulas
#' @export
survival_from_pd <- function(marginal) {
  marginal <- .fae_vector(marginal, "marginal PD", 0, 1)
  total <- cumsum(marginal)
  if (any(total > 1 + 1e-12)) .fae_abort("Marginal PD sum exceeds one")
  pmax(1 - total, 0)
}

#' @rdname core_formulas
#' @export
cumulative_pd_from_conditional <- function(conditional) {
  conditional <- .fae_vector(conditional, "conditional PD", 0, 1)
  1 - cumprod(1 - conditional)
}

#' @rdname core_formulas
#' @export
component_ecl <- function(marginal_pd, ead, lgd, eir, year) {
  .fae_number(marginal_pd, "PD", 0, 1) * .fae_number(ead, "EAD", 0) *
    .fae_number(lgd, "LGD", 0, 1) * discount_factor(eir, year)
}

#' @rdname core_formulas
#' @export
recovery_present_value <- function(recoveries, eir) {
  flows <- .fae_cashflows(recoveries, "recoveries")
  discount_factor(eir, 0)
  if (!nrow(flows)) return(0)
  sum(flows[, 2L] * vapply(flows[, 1L], function(t) discount_factor(eir, t), numeric(1)))
}

#' @rdname core_formulas
#' @export
cash_shortfall_ecl <- function(contractual, expected, eir) {
  contractual <- .fae_cashflows(contractual, "contractual", FALSE)
  expected <- .fae_cashflows(expected, "expected", FALSE)
  max(recovery_present_value(contractual, eir) - recovery_present_value(expected, eir), 0)
}

#' @rdname core_formulas
#' @export
modification_gain_loss <- function(gross_before, modified_cashflows, original_eir) {
  flows <- .fae_cashflows(modified_cashflows, "modified_cashflows", FALSE)
  recovery_present_value(flows, original_eir) - .fae_number(gross_before, "gross_before", 0)
}

#' @rdname core_formulas
#' @export
hedge_ineffectiveness <- function(hedging_change, hedged_change) {
  .fae_number(hedging_change, "hedging_change") + .fae_number(hedged_change, "hedged_change")
}

#' @rdname core_formulas
#' @export
ead_profile <- function(drawn, undrawn, ccf, prepayment) {
  lengths <- c(length(drawn), length(undrawn), length(ccf), length(prepayment))
  if (length(unique(lengths)) != 1L) .fae_abort("EAD vectors must have equal lengths")
  drawn <- .fae_vector(drawn, "drawn", 0)
  undrawn <- .fae_vector(undrawn, "undrawn", 0)
  ccf <- .fae_vector(ccf, "CCF", 0, 1)
  prepayment <- .fae_vector(prepayment, "prepayment", 0, 1)
  drawn * (1 - prepayment) + undrawn * ccf
}

#' @rdname core_formulas
#' @export
lgd_from_recoveries <- function(ead, recoveries, eir, costs = list()) {
  ead <- .fae_number(ead, "ead", 0)
  if (ead == 0) .fae_abort("LGD undefined for zero EAD")
  min(max((ead - recovery_present_value(recoveries, eir) +
             recovery_present_value(costs, eir)) / ead, 0), 1)
}

#' @rdname core_formulas
#' @export
provision_matrix_ecl <- function(exposure, loss_rate) {
  .fae_number(exposure, "exposure", 0) * .fae_number(loss_rate, "loss_rate", 0, 1)
}

#' @rdname core_formulas
#' @export
weighted_ecl <- function(scenario_losses, probabilities) {
  probabilities <- .fae_weights(probabilities)
  if (is.null(names(scenario_losses)) || !setequal(names(scenario_losses), names(probabilities)) ||
      length(scenario_losses) != length(probabilities)) .fae_abort("Scenario keys differ")
  losses <- .fae_vector(scenario_losses[names(probabilities)], "scenario losses", 0)
  sum(losses * probabilities)
}

#' @rdname core_formulas
#' @export
poci_allowance_change <- function(current_lifetime_ecl, initial_lifetime_ecl) {
  .fae_number(current_lifetime_ecl, "current ECL", 0) -
    .fae_number(initial_lifetime_ecl, "initial ECL", 0)
}

#' @rdname core_formulas
#' @export
liability_modification_test <- function(original_present_value, modified_present_value,
                                        qualitative_substantial = FALSE, tolerance = 0.1) {
  old <- .fae_number(original_present_value, "original PV", 0)
  new <- .fae_number(modified_present_value, "modified PV", 0)
  tolerance <- .fae_number(tolerance, "tolerance", 0, 1)
  qualitative_substantial <- .fae_flag(qualitative_substantial, "qualitative_substantial")
  if (old == 0) .fae_abort("Original PV must be positive")
  ratio <- abs(new - old) / old
  list(ratio = ratio, substantial = qualitative_substantial || ratio >= tolerance,
       threshold = tolerance)
}

#' @rdname core_formulas
#' @export
coverage_ratio <- function(allowance, gross_exposure) {
  allowance <- .fae_number(allowance, "allowance")
  gross_exposure <- .fae_number(gross_exposure, "gross exposure", 0)
  if (gross_exposure == 0) NA_real_ else allowance / gross_exposure
}

#' @rdname core_formulas
#' @export
benchmark_ecl <- function(iterations = 10000L) {
  if (!is.integer(iterations) && !(is.numeric(iterations) && length(iterations) == 1L &&
      is.finite(iterations) && iterations == as.integer(iterations))) {
    .fae_abort("iterations must be an integer in [1, 1000000]")
  }
  iterations <- as.integer(iterations)
  if (iterations < 1L || iterations > 1000000L) {
    .fae_abort("iterations must be an integer in [1, 1000000]")
  }
  start <- proc.time()[["elapsed"]]
  checksum <- sum(rep(component_ecl(0.02, 100000, 0.4, 0.05, 1), iterations))
  elapsed <- proc.time()[["elapsed"]] - start
  list(iterations = iterations, checksum = checksum, elapsed_seconds = elapsed,
       peak_bytes = NA_real_, formula_id = "F-ECL-001")
}
