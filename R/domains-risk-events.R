.fae_rows_for <- function(rows, instrument_id = NULL) {
  if (is.null(instrument_id)) return(rows)
  Filter(function(x) identical(as.character(x$instrument_id), as.character(instrument_id)), rows)
}

#' Impairment, Overlay, Event and Hedge Analyses
#'
#' Calculate scenario-based credit losses, govern credit enhancements and
#' overlays, process derecognition/modification/write-off/recovery events, and
#' measure documented hedge relationships and OCI reserves.
#'
#' @param collateral,instruments List of canonical collateral or instrument rows.
#' @param instrument Named canonical instrument row.
#' @param stage Explicit impairment stage.
#' @param pd_curves,lgd_profiles,ead_profiles Lists of rows keyed by
#'   `scenario_id` and positive integer `year`, with marginal PD, LGD and EAD.
#' @param probabilities Named scenario probabilities summing to one.
#' @param cashflows Optional canonical contractual and expected cash-flow rows.
#' @param reporting_date ISO reporting date required when an overlay is nonzero.
#' @param overlay Named governed overlay row.
#' @param amount Signed approved overlay amount or non-negative event amount,
#'   depending on the function.
#' @param allocation_weights Named allocation weights summing to one.
#' @param allocations Named signed allocated amounts.
#' @param tolerance Non-negative absolute reconciliation tolerance.
#' @param event Named approved event facts.
#' @param relationship Named hedge relationship and lifecycle facts; changes
#'   are cumulative and monetary amounts use one currency.
#' @param opening Signed opening OCI/hedge reserve.
#' @param hedge_results List of measured hedge rows.
#' @param fair_value_movement,effective_hedge_movement,costs,recycling,basis_adjustment
#'   Signed reserve movements in one currency.
#'
#' @return `scenario_ecl()` returns `list(details, summary)`, where `details`
#'   contains component rows and `summary` contains weighted, overlay and final
#'   ECL. Allocation and event functions return row lists; hedge functions
#'   return one result row. Rollforwards return a named reserve row.
#'
#' @details Stage 1 limits default events to year one; lifetime stages include
#'   every supplied annual curve point. Scenario losses are calculated before
#'   probability weighting. Direct cash-shortfall and provision-matrix methods
#'   remain explicit. Overlays require owner, thesis, approval, expiry and a
#'   double-count check. Event and hedge decisions consume documented inputs
#'   and never infer approvals or post to a ledger. Hedge effectiveness uses
#'   the lower-of absolute changes for cash-flow and net-investment hedges.
#'
#' @references International Accounting Standards Board (2014), IFRS 9,
#'   paragraphs 3.2--3.3, 5.5.17, B5.5.28--B5.5.55 and 6.4--6.6.
#'   Official source index: <https://www.ifrs.org/issued-standards/list-of-standards/ifrs-9-financial-instruments/>.
#'
#' @seealso [core_formulas], [classification_decisions], [build_journals()]
#'
#' @examples
#' collateral <- list(list(collateral_id="C", instrument_id="x", fair_value=100,
#'                         haircut=0.2, cost=5, allocated_amount=70, recovery_year=2))
#' allocate_credit_enhancements(collateral, list(list(instrument_id="x")))
#' instrument <- list(instrument_id="x", impairment_scope="GENERAL_ECL",
#'                    ecl_method="COMPONENT_PD_LGD_EAD", eir=0.05, overlay=0)
#' pd <- list(list(scenario_id="base", year=1, marginal_pd=0.02))
#' lgd <- list(list(scenario_id="base", year=1, lgd=0.4))
#' ead <- list(list(scenario_id="base", year=1, ead=100000))
#' scenario_ecl(instrument, "STAGE_1", pd, lgd, ead, c(base=1))$summary$final_ecl
#' governed <- list(overlay_id="O", overlay_owner="Risk", overlay_approved=TRUE,
#'                  overlay_expiry="2026-12-31", overlay_thesis="Emerging risk",
#'                  overlay_double_count_checked=TRUE, overlay=10)
#' validate_overlay(governed, "2026-12-31")
#' allocate_overlay(10, c(x=0.3, y=0.7))
#' reconcile_overlay(10, c(x=3, y=7))
#' source <- list(instrument_id="x", gross_amount=100)
#' transfer <- list(event_id="E", instrument_id="x", event_type="ASSET_TRANSFER",
#'                  approved=TRUE, reason_code="APPROVED", amount=10,
#'                  risks_rewards_transferred=TRUE)
#' assess_derecognition(transfer, source)
#' continuing_involvement(modifyList(transfer, list(risks_rewards_transferred=FALSE,
#'                        control_retained=TRUE, continuing_involvement_amount=4)), source)
#' process_modification(list(event_id="M", instrument_id="x",
#'                      event_type="ASSET_MODIFICATION_NO_DERECOGNITION", approved=TRUE,
#'                      reason_code="APPROVED", amount=0, modified_present_value=90,
#'                      derecognition_approved=FALSE), source)
#' process_writeoff(list(event_id="W", instrument_id="x", event_type="WRITEOFF",
#'                  approved=TRUE, reason_code="APPROVED", amount=10), source)
#' process_recovery(list(event_id="R", instrument_id="x", event_type="RECOVERY",
#'                  approved=TRUE, reason_code="APPROVED", amount=10), source)
#' hedge <- list(relationship_id="H", hedge_type="CASH_FLOW", hedging_change=120,
#'               hedged_change=-100, hedge_ratio=1, target_hedge_ratio=1,
#'               hedging_instrument_eligible=TRUE, hedged_item_eligible=TRUE,
#'               risk_component_eligible=TRUE, designation_documented=TRUE,
#'               risk_management_objective="Manage risk", designated=TRUE,
#'               economic_relationship=TRUE, credit_risk_dominates=FALSE,
#'               forecast_transaction_expected=TRUE)
#' validate_hedge_designation(hedge)
#' assess_hedge_effectiveness(hedge)
#' measured <- measure_hedge(hedge)
#' hedge_reserve_rollforward(10, list(measured))
#' oci_rollforward(10, fair_value_movement=5, recycling=2, basis_adjustment=1)
#' @name risk_event_hedge
NULL

#' @rdname risk_event_hedge
#' @export
allocate_credit_enhancements <- function(collateral, instruments) {
  ids <- vapply(instruments, function(x) as.character(x$instrument_id), character(1))
  limits <- numeric(); totals <- numeric(); result <- vector("list", length(collateral))
  for (i in seq_along(collateral)) {
    row <- .fae_required(collateral[[i]], c("collateral_id", "instrument_id", "fair_value",
      "haircut", "cost", "allocated_amount", "recovery_year"), "collateral")
    if (!row$instrument_id %in% ids) .fae_abort("Unknown collateral instrument")
    available <- max(.fae_number(row$fair_value, "fair_value", 0) *
      (1-.fae_number(row$haircut, "haircut", 0, 1)) - .fae_number(row$cost, "cost", 0), 0)
    id <- as.character(row$collateral_id)
    if (!is.na(limits[id]) && length(limits[id]) && limits[id] != available)
      .fae_abort("Inconsistent shared collateral capacity")
    limits[id] <- available
    prior_total <- if (id %in% names(totals)) totals[[id]] else 0
    totals[id] <- prior_total + .fae_number(row$allocated_amount, "allocated", 0)
    if (totals[id] > available + 1e-9) .fae_abort("Shared collateral over-allocation")
    result[[i]] <- list(collateral_id=row$collateral_id, instrument_id=row$instrument_id,
      available_protection=available, allocated_amount=as.numeric(row$allocated_amount),
      included_in_ecl=!isTRUE(row$separately_recognised), recovery_year=row$recovery_year,
      formula_id="F-COL-001", source_reference="IFRS9.B5.5.55")
  }
  result
}

#' @rdname risk_event_hedge
#' @export
scenario_ecl <- function(instrument, stage, pd_curves, lgd_profiles, ead_profiles,
                         probabilities, cashflows=list(), reporting_date=NULL) {
  instrument <- .fae_required(instrument, c("instrument_id", "impairment_scope"), "instrument")
  probabilities <- .fae_weights(probabilities)
  if (identical(stage, "NO_IMPAIRMENT_SCOPE") ||
      identical(instrument$impairment_scope, "NO_IMPAIRMENT_SCOPE") ||
      isTRUE(instrument$liability) || isTRUE(instrument$equity)) {
    return(list(details=list(), summary=list(instrument_id=instrument$instrument_id,
      stage="NO_IMPAIRMENT_SCOPE", weighted_ecl=0, overlay=0, final_ecl=0,
      formula_id="F-ECL-001")))
  }
  rate <- .fae_number(if (identical(stage, "POCI")) instrument$credit_adjusted_eir else
                        instrument$eir, "eir")
  overlay <- .fae_number(instrument$overlay %||% 0, "overlay")
  if (overlay != 0) {
    if (is.null(reporting_date)) .fae_abort("Overlay requires reporting_date")
    governance <- c(!is.null(instrument$overlay_id), !is.null(instrument$overlay_owner),
                    isTRUE(instrument$overlay_approved), !is.null(instrument$overlay_expiry),
                    !is.null(instrument$overlay_thesis),
                    isTRUE(instrument$overlay_double_count_checked))
    if (!all(governance) || instrument$overlay_expiry < reporting_date)
      .fae_abort("Overlay governance is invalid")
  }
  method <- instrument$ecl_method
  if (identical(method, "PROVISION_MATRIX")) {
    base <- provision_matrix_ecl(instrument$gross_amount, instrument$provision_rate)
    details <- list(list(instrument_id=instrument$instrument_id,
      scenario_id="PROVISION_MATRIX", year=0, marginal_pd=NULL, ead=instrument$gross_amount,
      lgd=NULL, discounted_ecl=base, formula_id="F-PM-001"))
    summary <- list(instrument_id=instrument$instrument_id, stage=stage, ecl_method=method,
      weighted_ecl=base, overlay=overlay, final_ecl=base+overlay, formula_id="F-PM-001",
      model_version_id="SYNTH_PROVISION_MATRIX_V1")
    return(list(details=details, summary=summary))
  }
  if (identical(method, "DIRECT_CASH_SHORTFALL")) {
    contractual <- Filter(function(x) isTRUE(x$contractual), cashflows)
    if (!length(contractual)) .fae_abort("Contractual cashflows are required")
    contractual <- lapply(contractual, function(x) c(x$year_fraction, x$amount))
    details <- list(); weighted <- 0
    for (sid in names(probabilities)) {
      expected <- Filter(function(x) isTRUE(x$expected) && identical(x$scenario_id, sid), cashflows)
      if (!length(expected)) .fae_abort(sprintf("Expected cashflows are missing for %s", sid))
      value <- cash_shortfall_ecl(contractual,
        lapply(expected, function(x) c(x$year_fraction, x$amount)), rate)
      weighted <- weighted + probabilities[[sid]]*value
      details[[length(details)+1L]] <- list(instrument_id=instrument$instrument_id,
        scenario_id=sid, year=0, marginal_pd=NULL, ead=NULL, lgd=NULL,
        discounted_ecl=value, formula_id="F-CS-001")
    }
    origin <- if (identical(stage, "POCI"))
      .fae_number(instrument$poci_initial_lifetime_ecl, "poci_initial_lifetime_ecl", 0) else 0
    measured <- if (identical(stage, "POCI")) poci_allowance_change(weighted, origin) else weighted
    return(list(details=details, summary=list(instrument_id=instrument$instrument_id,
      stage=stage, ecl_method=method, weighted_ecl=weighted, overlay=overlay,
      final_ecl=measured+overlay, poci_initial_lifetime_ecl=origin,
      formula_id="F-CS-001", model_version_id="SYNTH_CASH_SHORTFALL_V1")))
  }
  if (!identical(method, "COMPONENT_PD_LGD_EAD")) .fae_abort("Unknown ECL method")
  make_index <- function(rows, value) {
    keys <- vapply(rows, function(x) paste(x$scenario_id, x$year, sep="|"), character(1))
    if (anyDuplicated(keys)) .fae_abort("Duplicate scenario/year curve keys")
    setNames(lapply(rows, function(x) x[[value]]), keys)
  }
  pd <- make_index(pd_curves, "marginal_pd"); lgd <- make_index(lgd_profiles, "lgd")
  ead <- make_index(ead_profiles, "ead")
  if (!setequal(names(pd), names(lgd)) || !setequal(names(pd), names(ead)))
    .fae_abort("PD/LGD/EAD curve keys differ")
  details <- list(); weighted <- 0; horizon <- if (identical(stage, "STAGE_1")) 1 else Inf
  for (sid in names(probabilities)) {
    rows <- Filter(function(x) identical(x$scenario_id, sid), pd_curves)
    years <- sort(vapply(rows, function(x) .fae_number(x$year, "year", 1), numeric(1)))
    if (any(years != floor(years))) .fae_abort("Annual curve years must be positive integers")
    if (sum(vapply(rows, function(x) .fae_number(x$marginal_pd, "marginal_pd", 0, 1),
                   numeric(1))) > 1+1e-9) .fae_abort("Marginal PD sum exceeds one")
    years <- years[years <= horizon]
    if (!length(years)) .fae_abort(sprintf("Missing default horizon: %s", sid))
    scenario_loss <- 0
    for (year in years) {
      key <- paste(sid, year, sep="|")
      value <- component_ecl(pd[[key]], ead[[key]], lgd[[key]], rate, year)
      scenario_loss <- scenario_loss + value
      details[[length(details)+1L]] <- list(instrument_id=instrument$instrument_id,
        scenario_id=sid, year=year, marginal_pd=as.numeric(pd[[key]]),
        ead=as.numeric(ead[[key]]), lgd=as.numeric(lgd[[key]]), discounted_ecl=value,
        formula_id="F-ECL-001")
    }
    weighted <- weighted + probabilities[[sid]]*scenario_loss
  }
  origin <- if (identical(stage, "POCI"))
    .fae_number(instrument$poci_initial_lifetime_ecl %||% 0, "poci_initial_lifetime_ecl", 0) else 0
  measured <- if (identical(stage, "POCI")) poci_allowance_change(weighted, origin) else weighted
  list(details=details, summary=list(instrument_id=instrument$instrument_id, stage=stage,
    ecl_method=method, weighted_ecl=weighted, poci_initial_lifetime_ecl=origin,
    overlay=overlay, final_ecl=measured+overlay, formula_id="F-ECL-001",
    model_version_id="SYNTH_PD_LGD_EAD_V1"))
}

#' @rdname risk_event_hedge
#' @export
validate_overlay <- function(overlay, reporting_date) {
  overlay <- .fae_required(overlay, c("overlay_id", "overlay_owner", "overlay_approved",
    "overlay_expiry", "overlay_thesis", "overlay_double_count_checked", "overlay"), "overlay")
  amount <- .fae_number(overlay$overlay, "overlay")
  if (!isTRUE(overlay$overlay_approved) || !isTRUE(overlay$overlay_double_count_checked) ||
      as.Date(overlay$overlay_expiry) < as.Date(reporting_date)) .fae_abort("Overlay is not valid")
  list(overlay_id=overlay$overlay_id, amount=amount, status="VALID")
}

#' @rdname risk_event_hedge
#' @export
allocate_overlay <- function(amount, allocation_weights) {
  amount <- .fae_number(amount, "amount"); w <- .fae_weights(allocation_weights)
  result <- amount*w; as.list(result)
}

#' @rdname risk_event_hedge
#' @export
reconcile_overlay <- function(amount, allocations, tolerance=1e-9) {
  reconcile_ledger(.fae_number(amount, "amount"),
                   sum(.fae_vector(unlist(allocations), "allocations")), tolerance)
}

.fae_process_event <- function(event, instrument, kinds) {
  event <- .fae_required(event, c("event_id", "instrument_id", "event_type", "approved",
                                  "reason_code"), "event")
  instrument <- .fae_required(instrument, c("instrument_id", "gross_amount"), "instrument")
  if (!event$event_type %in% kinds) .fae_abort("Wrong event type for this function")
  if (!identical(event$instrument_id, instrument$instrument_id))
    .fae_abort("Event/instrument key mismatch")
  if (!isTRUE(event$approved)) .fae_abort("Event is not approved")
  kind <- event$event_type; amount <- .fae_number(event$amount %||% 0, "amount", 0)
  result <- list(event_id=event$event_id, instrument_id=event$instrument_id, event_type=kind,
    amount=amount, derecognition_result="NOT_APPLICABLE", continuing_involvement=0,
    modification_result=0, pnl_effect=0, reason_code=event$reason_code, formula_id=NULL)
  if (kind %in% c("ASSET_MODIFICATION_NO_DERECOGNITION", "ASSET_MODIFICATION_DERECOGNITION")) {
    expected <- identical(kind, "ASSET_MODIFICATION_DERECOGNITION")
    if (!identical(isTRUE(event$derecognition_approved), expected))
      .fae_abort("Asset modification derecognition approval is inconsistent")
    result$derecognition_result <- if (expected) "DERECOGNISED_AND_NEW_RECOGNITION" else "NO_DERECOGNITION"
    result$modification_result <- .fae_number(event$modified_present_value,
      "modified_present_value", 0) - .fae_number(instrument$gross_amount, "gross_amount", 0)
    result$pnl_effect <- result$modification_result; result$formula_id <- "F-MOD-001"
  } else if (kind %in% c("LIABILITY_MODIFICATION_DERECOGNITION",
                         "LIABILITY_MODIFICATION_NO_DERECOGNITION")) {
    substantial <- isTRUE(event$qualitative_substantial) ||
      .fae_number(event$ten_percent_ratio %||% 0, "ten_percent_ratio", 0) >= 0.1
    expected <- identical(kind, "LIABILITY_MODIFICATION_DERECOGNITION")
    if (expected != substantial || expected != isTRUE(event$derecognition_approved))
      .fae_abort("Liability modification conflicts with the substantial-modification test")
    result$derecognition_result <- if (substantial) "DERECOGNISED_AND_NEW_RECOGNITION" else "NO_DERECOGNITION"
    result$modification_result <- .fae_number(event$modified_present_value,
      "modified_present_value", 0) - .fae_number(instrument$gross_amount, "gross_amount", 0)
    result$pnl_effect <- -result$modification_result; result$formula_id <- "F-MOD-001"
  } else if (identical(kind, "ASSET_TRANSFER")) {
    if (identical(event$pass_through_arrangement, FALSE)) .fae_abort("Pass-through criteria failed")
    if (isTRUE(event$risks_rewards_transferred)) decision <- "FULL_DERECOGNITION"
    else if (isTRUE(event$risks_rewards_retained)) decision <- "NO_DERECOGNITION"
    else if (isTRUE(event$control_retained)) {
      decision <- "CONTINUING_INVOLVEMENT"
      result$continuing_involvement <- .fae_number(event$continuing_involvement_amount %||% 0,
                                                   "continuing_involvement", 0)
    } else decision <- "FULL_DERECOGNITION_CONTROL_LOST"
    if (isTRUE(event$partial_transfer) && amount <= 0) .fae_abort("Partial transfer has no amount")
    result$derecognition_result <- decision; result$formula_id <- "F-DEREC-001"
  } else if (identical(kind, "WRITEOFF")) {
    result$derecognition_result <- "WRITEOFF"; result$formula_id <- "F-WO-001"
  } else if (identical(kind, "RECOVERY")) {
    result$derecognition_result <- "POST_WRITEOFF_RECOVERY"; result$pnl_effect <- amount
    result$formula_id <- "F-REC-001"
  } else .fae_abort("Unknown event type")
  result
}

#' @rdname risk_event_hedge
#' @export
assess_derecognition <- function(event, instrument)
  .fae_process_event(event, instrument, "ASSET_TRANSFER")

#' @rdname risk_event_hedge
#' @export
continuing_involvement <- function(event, instrument) {
  result <- assess_derecognition(event, instrument)
  if (result$continuing_involvement < 0 || result$continuing_involvement > result$amount)
    .fae_abort("Continuing involvement is outside the transferred amount")
  result
}

#' @rdname risk_event_hedge
#' @export
process_modification <- function(event, instrument) .fae_process_event(event, instrument, c(
  "ASSET_MODIFICATION_NO_DERECOGNITION", "ASSET_MODIFICATION_DERECOGNITION",
  "LIABILITY_MODIFICATION_NO_DERECOGNITION", "LIABILITY_MODIFICATION_DERECOGNITION"))

#' @rdname risk_event_hedge
#' @export
process_writeoff <- function(event, instrument) {
  .fae_number(event$amount, "amount", 0, .fae_number(instrument$gross_amount, "gross_amount", 0))
  .fae_process_event(event, instrument, "WRITEOFF")
}

#' @rdname risk_event_hedge
#' @export
process_recovery <- function(event, instrument) {
  .fae_number(event$amount, "amount", 0)
  .fae_process_event(event, instrument, "RECOVERY")
}

#' @rdname risk_event_hedge
#' @export
measure_hedge <- function(relationship) {
  relationship <- .fae_required(relationship, c("relationship_id", "hedge_type",
    "hedging_change", "hedged_change", "hedge_ratio", "target_hedge_ratio"), "relationship")
  if (!relationship$hedge_type %in% c("FAIR_VALUE", "CASH_FLOW", "NET_INVESTMENT"))
    .fae_abort("Unknown hedge type")
  complete <- all(vapply(c("hedging_instrument_eligible", "hedged_item_eligible",
    "risk_component_eligible", "designation_documented", "risk_management_objective"),
    function(x) isTRUE(relationship[[x]]) || (identical(x,"risk_management_objective") &&
      !is.null(relationship[[x]]) && nzchar(relationship[[x]])), logical(1)))
  if (isTRUE(relationship$nature_dependent_electricity_hedge)) complete <- complete &&
    isTRUE(relationship$variable_nominal_volume) && isTRUE(relationship$volume_assumptions_documented) &&
    isTRUE(relationship$forecast_transaction_expected) && identical(relationship$hedge_type,"CASH_FLOW")
  eligible <- complete && isTRUE(relationship$designated) &&
    isTRUE(relationship$economic_relationship) && !isTRUE(relationship$credit_risk_dominates) &&
    .fae_number(relationship$hedge_ratio, "hedge_ratio") > 0
  empty <- list(relationship_id=relationship$relationship_id, status="REJECTED",
    ineffectiveness_pnl=0, oci_effective=0, cost_of_hedging_reserve=0,
    recycle_amount=0, basis_adjustment=0, formula_id="F-HDG-001")
  if (!eligible) return(empty)
  gain <- .fae_number(relationship$hedging_change, "hedging_change")
  item <- .fae_number(relationship$hedged_change, "hedged_change")
  ineffective <- hedge_ineffectiveness(gain, item); oci <- 0
  if (!identical(relationship$hedge_type, "FAIR_VALUE")) {
    oci <- if (gain*item < 0) sign(gain)*min(abs(gain),abs(item)) else 0
    ineffective <- gain-oci
  }
  status <- if (isTRUE(relationship$discontinue) ||
    (identical(relationship$hedge_type,"CASH_FLOW") && !isTRUE(relationship$forecast_transaction_expected)))
    "DISCONTINUED" else if (isTRUE(relationship$rebalancing_required) ||
      abs(relationship$hedge_ratio-relationship$target_hedge_ratio)>1e-12) "REBALANCE_REQUIRED" else "ELIGIBLE"
  costs <- .fae_number(relationship$option_time_value_change %||% 0,"option_time_value_change")+
    .fae_number(relationship$forward_element_change %||% 0,"forward_element_change")+
    .fae_number(relationship$basis_spread_change %||% 0,"basis_spread_change")
  list(relationship_id=relationship$relationship_id, hedge_type=relationship$hedge_type,
    status=status, nature_dependent_electricity_hedge=isTRUE(relationship$nature_dependent_electricity_hedge),
    ineffectiveness_pnl=ineffective, oci_effective=oci, cost_of_hedging_reserve=costs,
    recycle_amount=.fae_number(relationship$recycle_amount %||% 0,"recycle_amount"),
    basis_adjustment=.fae_number(relationship$basis_adjustment %||% 0,"basis_adjustment"),
    formula_id="F-HDG-001", source_reference="IFRS9.6.4-6.5_6.10")
}

#' @rdname risk_event_hedge
#' @export
validate_hedge_designation <- function(relationship) {
  x <- measure_hedge(relationship)
  list(relationship_id=x$relationship_id, eligible=!identical(x$status,"REJECTED"), status=x$status)
}

#' @rdname risk_event_hedge
#' @export
assess_hedge_effectiveness <- function(relationship) measure_hedge(relationship)

#' @rdname risk_event_hedge
#' @export
oci_rollforward <- function(opening, fair_value_movement=0, effective_hedge_movement=0,
                            costs=0, recycling=0, basis_adjustment=0) {
  values <- vapply(list(opening, fair_value_movement, effective_hedge_movement, costs,
                        recycling, basis_adjustment), .fae_number, numeric(1), name="movement")
  list(opening_reserve=values[1], fair_value_movement=values[2],
    effective_hedge_movement=values[3], cost_of_hedging_movement=values[4],
    recycling=values[5], basis_adjustment=values[6],
    closing_reserve=values[1]+values[2]+values[3]+values[4]-values[5]-values[6])
}

#' @rdname risk_event_hedge
#' @export
hedge_reserve_rollforward <- function(opening, hedge_results) {
  total <- function(key) sum(vapply(hedge_results, function(x) .fae_number(x[[key]],key), numeric(1)))
  oci_rollforward(opening, effective_hedge_movement=total("oci_effective"),
    costs=total("cost_of_hedging_reserve"), recycling=total("recycle_amount"),
    basis_adjustment=total("basis_adjustment"))
}
