.fae_required <- function(row, fields, object = "row") {
  if (!is.list(row)) .fae_abort(sprintf("%s must be a named list", object))
  missing <- fields[vapply(fields, function(key) {
    is.null(row[[key]]) || !length(row[[key]]) || is.na(row[[key]]) || identical(row[[key]], "")
  }, logical(1))]
  if (length(missing)) {
    .fae_abort(sprintf("Missing required field(s): %s", paste(missing, collapse = ", ")))
  }
  row
}

.fae_sppi <- function(features) {
  if (!is.null(features$sppi_override)) {
    if (is.null(features$approval_id) || !nzchar(features$approval_id)) {
      .fae_abort("SPPI override requires an approval_id")
    }
    return(list(pass = isTRUE(features$sppi_override), reason = "APPROVED_OVERRIDE"))
  }
  checks <- c("principal_consistent", "basic_interest", "modified_time_value_pass",
              "prepayment_compensation_reasonable", "extension_basic",
              "non_recourse_pass", "cli_lookthrough_pass", "contingent_feature_basic")
  if (isTRUE(features$de_minimis_or_non_genuine)) features$contingent_feature_basic <- TRUE
  if (any(vapply(checks, function(key) is.null(features[[key]]) || is.na(features[[key]]),
                 logical(1)))) .fae_abort("Complete SPPI features are required")
  passed <- all(vapply(checks, function(key) isTRUE(features[[key]]), logical(1))) &&
    !isTRUE(features$leverage)
  list(pass = passed,
       reason = if (passed) "ALL_BASIC_LENDING_FEATURES" else "NON_BASIC_OR_LEVERAGED_FEATURE")
}

#' Classification, Scope, Measurement and Staging Decisions
#'
#' Native row-level decisions for scope and recognition, business-model and
#' contractual cash-flow assessment, classification, initial/fair-value
#' measurement, own-credit presentation, interest revenue, impairment scope,
#' staging and controlled IBOR interfaces.
#'
#' @param instrument Named list of canonical instrument facts. Monetary amounts
#'   use the instrument's currency; required fields depend on the function.
#' @param model Approved business-model judgement: `HOLD_TO_COLLECT`,
#'   `HOLD_TO_COLLECT_AND_SELL`, or `OTHER`.
#' @param features Named list containing the eight documented basic-lending
#'   checks, leverage flag and any explicitly approved override.
#' @param scope_decision Explicit scope result, normally `IN_IFRS9_SCOPE`.
#' @param category Classification category used for measurement presentation.
#' @param market Named list of supplied fair-value and valuation-control facts.
#' @param stage Explicit impairment stage.
#' @param allowance Allowance in the instrument's currency; signed POCI changes
#'   are permitted.
#' @param facts Named staging facts including instrument ID, impairment scope,
#'   days past due and origination/current ratings.
#' @param policy Explicit versioned policy list. `NULL` selects the fixed
#'   packaged policy `EU_FINANCIAL_INSTRUMENTS_2026_V1`.
#' @param stage_results List of rows with `prior_stage` and `stage`.
#' @param applicable One non-missing logical IBOR applicability judgement.
#' @param policy_reference,approval_reference Non-empty identifiers for the
#'   institution's policy and case approval.
#'
#' @return A named list containing the requested decision, amount, reason,
#'   formula/rule ID and source reference. `stage_movements()` returns a list of
#'   rows with prior stage, current stage and instrument count.
#'
#' @details These functions validate supplied facts and approved judgements;
#'   they do not infer a business model, designation, override or institutional
#'   approval. Initial measurement adds transaction costs for financial assets
#'   outside FVTPL and subtracts them for liabilities. Interest is calculated
#'   on gross carrying amount in stages 1/2, net amount in stage 3 and the
#'   credit-adjusted net basis for POCI. The 30- and 90-day thresholds remain
#'   rebuttable/policy-driven and credit impairment takes precedence over SICR.
#'
#' @references International Accounting Standards Board (2014), IFRS 9,
#'   paragraphs 2.1--2.7, 3.1, 4.1--4.2, 5.1, 5.4, 5.5 and 5.7; amendments on
#'   classification and measurement applicable in the packaged 2026 policy.
#'   Official source index: <https://www.ifrs.org/issued-standards/list-of-standards/ifrs-9-financial-instruments/>.
#'
#' @seealso [core_formulas], [scenario_ecl()], [reference_data]
#'
#' @examples
#' instrument <- list(
#'   instrument_id="x", instrument_type="LOAN", scope_assessment="IN_IFRS9_SCOPE",
#'   recognition_status="RECOGNISED", trade_date_policy="TRADE_DATE",
#'   regular_way=FALSE, business_model="HOLD_TO_COLLECT", liability=FALSE,
#'   equity=FALSE, fair_value=100, transaction_costs=2, transaction_price=100,
#'   impairment_scope="GENERAL_ECL", gross_amount=100, eir=0.1
#' )
#' features <- list(
#'   principal_consistent=TRUE, basic_interest=TRUE, leverage=FALSE,
#'   modified_time_value_pass=TRUE, prepayment_compensation_reasonable=TRUE,
#'   extension_basic=TRUE, non_recourse_pass=TRUE, cli_lookthrough_pass=TRUE,
#'   contingent_feature_basic=TRUE, business_model_change=FALSE
#' )
#' assess_scope(instrument)
#' assess_recognition(instrument)
#' assess_business_model("HOLD_TO_COLLECT")
#' assess_sppi(features)
#' classify_instrument(instrument, features)
#' assess_reclassification(instrument, features)
#' initial_measurement(instrument, "AMORTISED_COST")
#' market <- list(instrument_id="x", fair_value=110, total_fv_change=10,
#'                own_credit_change=4, ifrs13_level="2", valuation_model="DCF",
#'                valuation_status="APPROVED")
#' present_fair_value(instrument, market, "FVOCI_EQUITY")
#' split_own_credit(market, "FVTPL_LIABILITY")
#' interest_revenue(instrument, "STAGE_3", 20)
#' impairment_scope(instrument)
#' stage_facts <- list(instrument_id="x", impairment_scope="GENERAL_ECL",
#'                     days_past_due=0, rating_orig=3, rating_current=3)
#' assess_stage(stage_facts)
#' assess_sicr(stage_facts)
#' stage_movements(list(list(prior_stage="STAGE_1", stage="STAGE_2")))
#' assess_ibor_relief(FALSE, "IBOR_PHASE2_POLICY_V1", "CASE_APPROVAL_1")
#' @name classification_decisions
NULL

#' @rdname classification_decisions
#' @export
assess_scope <- function(instrument) {
  instrument <- .fae_required(instrument,
                              c("instrument_id", "scope_assessment", "recognition_status",
                                "trade_date_policy"), "instrument")
  scope <- instrument$scope_assessment
  if (identical(scope, "OUTSIDE_IFRS9_OWN_USE")) {
    criteria <- isTRUE(instrument$nature_dependent_contract) &&
      isTRUE(instrument$purchaser_net_buyer) &&
      isTRUE(instrument$quantity_consistent_expected_usage) &&
      isTRUE(instrument$sale_timing_uncontrollable) &&
      isTRUE(instrument$repurchase_within_reasonable_time)
    if (!isTRUE(instrument$own_use) ||
        !identical(instrument$instrument_type, "NATURE_DEPENDENT_ELECTRICITY") || !criteria) {
      .fae_abort("Own-use exception is not consistently documented")
    }
    decision <- "NO_IFRS9_SCOPE"
    reason <- "IFRS9.2.4_B2.7-B2.8"
  } else if (identical(scope, "IN_IFRS9_SCOPE")) {
    decision <- "IN_IFRS9_SCOPE"
    reason <- "IFRS9.2.1-2.7"
  } else if (is.character(scope) && startsWith(scope, "OUTSIDE_IFRS9_")) {
    standards <- c("IAS32", "IFRS7", "IFRS13", "IFRS15", "IFRS16", "IFRS17",
                   "IAS21", "IAS28", "IAS36", "IAS37", "IFRS10")
    if (!instrument$interface_standard %in% standards ||
        is.null(instrument$interface_decision_id) || !isTRUE(instrument$interface_approved)) {
      .fae_abort("Scope interface requires a supported standard and documented approval")
    }
    decision <- "NO_IFRS9_SCOPE"
    reason <- paste("CONTROLLED_INTERFACE", instrument$interface_standard,
                    instrument$interface_decision_id, sep="_")
  } else .fae_abort("Unknown scope decision")
  if (identical(decision, "IN_IFRS9_SCOPE") &&
      !instrument$recognition_status %in% c("RECOGNISED", "PENDING_REGULAR_WAY")) {
    .fae_abort("In-scope instrument has no permitted recognition status")
  }
  if (isTRUE(instrument$regular_way) &&
      !instrument$trade_date_policy %in% c("TRADE_DATE", "SETTLEMENT_DATE")) {
    .fae_abort("Regular-way policy is missing")
  }
  epay <- "NOT_APPLICABLE"
  if (isTRUE(instrument$electronic_payment_election)) {
    criteria <- isTRUE(instrument$liability) && isTRUE(instrument$payment_irrevocable) &&
      !isTRUE(instrument$withdrawal_access_practical) &&
      isTRUE(instrument$settlement_risk_insignificant) &&
      isTRUE(instrument$settlement_short_period)
    if (!criteria) .fae_abort("Electronic-payment election criteria are incomplete")
    epay <- "ELECTRONIC_PAYMENT_EXCEPTION_APPLIED"
  }
  list(instrument_id=instrument$instrument_id, scope_decision=decision,
       recognition_event=instrument$recognition_status,
       regular_way_policy=if (isTRUE(instrument$regular_way)) instrument$trade_date_policy else "NOT_APPLICABLE",
       electronic_payment_result=epay,
       controlled_interface=instrument$interface_standard %||% "NOT_APPLICABLE",
       interface_decision_id=instrument$interface_decision_id,
       decision_rule_id="SCOPE-001", source_reference=reason)
}

#' @rdname classification_decisions
#' @export
assess_recognition <- function(instrument) assess_scope(instrument)

#' @rdname classification_decisions
#' @export
assess_business_model <- function(model) {
  if (!is.character(model) || length(model) != 1L ||
      !model %in% c("HOLD_TO_COLLECT", "HOLD_TO_COLLECT_AND_SELL", "OTHER")) {
    .fae_abort("Unknown business model")
  }
  list(business_model=model, judgement_required=TRUE,
       source_reference="IFRS9.4.1.1-B4.1.2")
}

#' @rdname classification_decisions
#' @export
assess_sppi <- function(features) {
  x <- .fae_sppi(features)
  list(sppi_pass=x$pass, reason=x$reason, source_reference="IFRS9.4.1.2_B4.1")
}

#' @rdname classification_decisions
#' @export
classify_instrument <- function(instrument, features, scope_decision="IN_IFRS9_SCOPE") {
  instrument <- .fae_required(instrument, c("instrument_id", "instrument_type"), "instrument")
  if (identical(scope_decision, "NO_IFRS9_SCOPE")) {
    return(list(instrument_id=instrument$instrument_id, classification="NO_IFRS9_SCOPE",
                sppi_result="NOT_APPLICABLE", sppi_reason="NOT_APPLICABLE",
                decision_rule_id="SCOPE-001", source_reference="IFRS9.2"))
  }
  sppi <- .fae_sppi(features)
  if ((isTRUE(instrument$fvoci_equity_election) || isTRUE(instrument$fair_value_option)) &&
      is.null(instrument$designation_approval_id)) .fae_abort("Designation approval is required")
  if (isTRUE(features$business_model_change) &&
      (isTRUE(instrument$liability) || isTRUE(instrument$equity))) {
    .fae_abort("Reclassification is not permitted for liability/equity instruments")
  }
  itype <- instrument$instrument_type
  if (identical(itype, "FINANCIAL_GUARANTEE")) {
    category <- "FINANCIAL_GUARANTEE"; reason <- "IFRS9.4.2.1(c)_5.5"
  } else if (identical(itype, "LOAN_COMMITMENT")) {
    category <- "LOAN_COMMITMENT"; reason <- "IFRS9.2.1(g)_5.5"
  } else if (isTRUE(instrument$liability)) {
    category <- if (isTRUE(instrument$hybrid_contract) &&
                    identical(instrument$host_contract_type, "FINANCIAL_LIABILITY") &&
                    isTRUE(instrument$embedded_derivative_separated)) {
      "AC_HOST_PLUS_SEPARATE_DERIVATIVE"
    } else if (isTRUE(instrument$held_for_trading) || isTRUE(instrument$fair_value_option)) {
      "FVTPL_LIABILITY"
    } else "AMORTISED_COST_LIABILITY"
    reason <- "IFRS9.4.2"
  } else if (isTRUE(instrument$equity)) {
    if (isTRUE(instrument$fvoci_equity_election) && isTRUE(instrument$held_for_trading)) {
      .fae_abort("Trading equity cannot elect FVOCI")
    }
    category <- if (isTRUE(instrument$fvoci_equity_election)) "FVOCI_EQUITY" else "FVTPL"
    reason <- if (isTRUE(instrument$fvoci_equity_election)) "IFRS9.5.7.5" else "IFRS9.4.1.4"
  } else if (isTRUE(instrument$fair_value_option)) {
    category <- "FVTPL"; reason <- "IFRS9.4.1.5_ACCOUNTING_MISMATCH"
  } else if (!sppi$pass || identical(instrument$business_model, "OTHER")) {
    category <- "FVTPL"; reason <- "IFRS9.4.1.4"
  } else if (identical(instrument$business_model, "HOLD_TO_COLLECT_AND_SELL")) {
    category <- "FVOCI_DEBT"; reason <- "IFRS9.4.1.2A"
  } else if (identical(instrument$business_model, "HOLD_TO_COLLECT")) {
    category <- "AMORTISED_COST"; reason <- "IFRS9.4.1.2"
  } else .fae_abort("Unknown business model")
  list(instrument_id=instrument$instrument_id, classification=category,
       sppi_result=if (sppi$pass) "PASS" else "FAIL", sppi_reason=sppi$reason,
       reclassification_status=if (isTRUE(features$business_model_change))
         "PROSPECTIVE_RECLASSIFICATION" else "NO_RECLASSIFICATION",
       decision_rule_id="CLS-001", source_reference=reason)
}

#' @rdname classification_decisions
#' @export
assess_reclassification <- function(instrument, features) {
  x <- classify_instrument(instrument, features)
  x[c("instrument_id", "classification", "reclassification_status", "source_reference")]
}

#' @rdname classification_decisions
#' @export
initial_measurement <- function(instrument, category) {
  instrument <- .fae_required(instrument,
                              c("instrument_id", "fair_value", "transaction_costs",
                                "transaction_price"), "instrument")
  fv <- .fae_number(instrument$fair_value, "fair_value")
  costs <- .fae_number(instrument$transaction_costs, "transaction_costs", 0)
  if (category %in% c("FVTPL", "FVTPL_LIABILITY")) {
    initial <- fv; treatment <- "P&L"
  } else if (identical(category, "NO_IFRS9_SCOPE")) {
    initial <- 0; treatment <- "NOT_APPLICABLE"
  } else {
    initial <- fv + if (isTRUE(instrument$liability)) -costs else costs
    treatment <- "IN_EFFECTIVE_INTEREST_RATE"
  }
  list(instrument_id=instrument$instrument_id, initial_carrying_amount=initial,
       transaction_cost_treatment=treatment,
       day_one_difference=fv - .fae_number(instrument$transaction_price, "transaction_price"),
       formula_id="F-INIT-001", source_reference="IFRS9.5.1.1-5.1.3")
}

#' @rdname classification_decisions
#' @export
split_own_credit <- function(market, category) {
  market <- .fae_required(market, c("instrument_id", "total_fv_change", "own_credit_change"),
                          "market")
  total <- .fae_number(market$total_fv_change, "total_fv_change")
  own <- if (identical(category, "FVTPL_LIABILITY"))
    .fae_number(market$own_credit_change, "own_credit_change") else 0
  mismatch <- isTRUE(market$own_credit_oci_mismatch)
  own_oci <- if (mismatch) 0 else own
  list(instrument_id=market$instrument_id, total_fv_change=total,
       own_credit_oci=own_oci, own_credit_pnl_due_to_mismatch=if (mismatch) own else 0,
       other_fv_change_pnl=total-own_oci, formula_id="F-OWNCR-001",
       source_reference="IFRS9.5.7.7-5.7.9")
}

#' @rdname classification_decisions
#' @export
present_fair_value <- function(instrument, market, category) {
  instrument <- .fae_required(instrument, "instrument_id", "instrument")
  market <- .fae_required(market, c("fair_value", "ifrs13_level", "valuation_model",
                                    "valuation_status"), "market")
  total <- .fae_number(market$total_fv_change %||% 0, "total_fv_change")
  if (category %in% c("FVTPL", "FVTPL_LIABILITY")) {
    own_market <- market
    own_market$instrument_id <- instrument$instrument_id
    own_market$own_credit_change <- market$own_credit_change %||% 0
    own <- split_own_credit(own_market, category)
    pnl <- own$other_fv_change_pnl; oci <- own$own_credit_oci; recycling <- "NOT_APPLICABLE"
  } else if (identical(category, "FVOCI_DEBT")) {
    pnl <- 0; oci <- total; recycling <- "RECYCLE_ON_DERECOGNITION"
  } else if (identical(category, "FVOCI_EQUITY")) {
    pnl <- 0; oci <- total; recycling <- "NO_RECYCLING"
  } else { pnl <- 0; oci <- 0; recycling <- "NOT_APPLICABLE" }
  list(instrument_id=instrument$instrument_id, classification=category,
       fair_value=.fae_number(market$fair_value, "fair_value"), total_fv_change=total,
       pnl_fv_change=pnl, oci_fv_change=oci, recycling_policy=recycling,
       ifrs13_level=market$ifrs13_level, valuation_model=market$valuation_model,
       valuation_status=market$valuation_status, formula_id="F-FV-001",
       source_reference="IFRS9.5.7_IFRS13")
}

#' @rdname classification_decisions
#' @export
interest_revenue <- function(instrument, stage, allowance) {
  instrument <- .fae_required(instrument, c("instrument_id", "gross_amount", "eir"), "instrument")
  allowance <- .fae_number(allowance, "allowance")
  gross <- .fae_number(instrument$gross_amount, "gross_amount", 0)
  rate <- if (isTRUE(instrument$poci))
    .fae_number(instrument$credit_adjusted_eir, "credit_adjusted_eir") else
      .fae_number(instrument$eir, "eir")
  basis <- if (stage %in% c("STAGE_3", "POCI")) max(gross-allowance, 0) else gross
  method <- if (identical(stage, "STAGE_3")) "NET" else if (identical(stage, "POCI"))
    "CREDIT_ADJUSTED" else "GROSS"
  list(instrument_id=instrument$instrument_id, interest_basis=basis, interest_rate=rate,
       interest_revenue=basis*rate, interest_method=method, formula_id="F-AC-001",
       source_reference="IFRS9.5.4.1")
}

#' @rdname classification_decisions
#' @export
impairment_scope <- function(instrument) {
  instrument <- .fae_required(instrument, c("instrument_id", "impairment_scope"), "instrument")
  approach <- if (isTRUE(instrument$liability) || isTRUE(instrument$equity) ||
                   identical(instrument$impairment_scope, "NO_IMPAIRMENT_SCOPE")) {
    "NO_IMPAIRMENT_SCOPE"
  } else if (isTRUE(instrument$poci)) "POCI"
  else if (isTRUE(instrument$simplified)) "SIMPLIFIED_LIFETIME" else "GENERAL"
  list(instrument_id=instrument$instrument_id, approach=approach)
}

#' @rdname classification_decisions
#' @export
assess_stage <- function(facts, policy=NULL) {
  facts <- .fae_required(facts, c("instrument_id", "impairment_scope", "days_past_due",
                                  "rating_orig", "rating_current"), "facts")
  policy <- if (is.null(policy)) get_policy() else policy
  required <- c("reporting_date", "default_backstop_days", "past_due_sicr_days",
                "rating_notch_sicr", "absolute_lifetime_pd_sicr",
                "relative_lifetime_pd_sicr", "cure_months", "probation_months")
  .fae_required(policy, required, "policy")
  override <- facts$sicr_override
  if (!is.null(override) || isTRUE(facts$sicr_rebuttal)) {
    if (!isTRUE(facts$override_approved) || is.null(facts$override_reason) ||
        is.null(facts$override_expiry) || facts$override_expiry < policy$reporting_date) {
      .fae_abort("Stage override/rebuttal requires a current documented approval")
    }
  }
  if (identical(facts$impairment_scope, "NO_IMPAIRMENT_SCOPE")) {
    stage <- "NO_IMPAIRMENT_SCOPE"; reason <- "OUTSIDE_IMPAIRMENT_REQUIREMENTS"
  } else if (isTRUE(facts$poci)) { stage <- "POCI"; reason <- "POCI_AT_ORIGINATION"
  } else if (isTRUE(facts$simplified)) {
    stage <- "SIMPLIFIED_LIFETIME"; reason <- "SIMPLIFIED_APPROACH"
  } else if (isTRUE(facts$credit_impaired) || isTRUE(facts$unlikely_to_pay) ||
             facts$days_past_due >= policy$default_backstop_days) {
    stage <- "STAGE_3"; reason <- "CREDIT_IMPAIRED_OR_DEFAULT_BACKSTOP"
  } else if (isTRUE(policy$low_credit_risk_relief) && isTRUE(facts$low_credit_risk)) {
    stage <- "STAGE_1"; reason <- "LOW_CREDIT_RISK_RELIEF"
  } else {
    triggers <- character()
    if (isTRUE(override)) triggers <- c(triggers, "APPROVED_OVERRIDE")
    if (isTRUE(facts$watchlist)) triggers <- c(triggers, "WATCHLIST")
    if (isTRUE(facts$forbearance)) triggers <- c(triggers, "FORBEARANCE")
    if (isTRUE(facts$forward_looking_sicr)) triggers <- c(triggers, "FORWARD_LOOKING_SICR")
    if (isTRUE(facts$collective_assessment)) triggers <- c(triggers, "COLLECTIVE_TOP_DOWN")
    if (facts$days_past_due >= policy$past_due_sicr_days && !isTRUE(facts$sicr_rebuttal))
      triggers <- c(triggers, "PAST_DUE_REBUTTABLE_PRESUMPTION")
    if (facts$rating_current - facts$rating_orig >= policy$rating_notch_sicr)
      triggers <- c(triggers, "RATING_MIGRATION")
    if (!is.null(facts$lifetime_pd_orig) && !is.null(facts$lifetime_pd_current)) {
      if (facts$lifetime_pd_current - facts$lifetime_pd_orig >= policy$absolute_lifetime_pd_sicr)
        triggers <- c(triggers, "ABSOLUTE_LIFETIME_PD_CHANGE")
      if (facts$lifetime_pd_orig > 0 &&
          facts$lifetime_pd_current / facts$lifetime_pd_orig >= policy$relative_lifetime_pd_sicr)
        triggers <- c(triggers, "RELATIVE_LIFETIME_PD_CHANGE")
    }
    if ((facts$months_since_cure %||% 999) < policy$cure_months)
      triggers <- c(triggers, "CURE_PERIOD")
    if ((facts$months_since_forbearance %||% 999) < policy$probation_months)
      triggers <- c(triggers, "FORBEARANCE_PROBATION")
    stage <- if (length(triggers)) "STAGE_2" else "STAGE_1"
    reason <- if (length(triggers)) paste(triggers, collapse="+") else "NO_SICR"
  }
  prior <- facts$prior_stage %||% "NOT_AVAILABLE"
  list(instrument_id=facts$instrument_id, prior_stage=prior, stage=stage,
       stage_movement=paste(prior, stage, sep="->"), stage_reason=reason,
       decision_rule_id="STG-001", source_reference="IFRS9.5.5.3-5.5.11")
}

#' @rdname classification_decisions
#' @export
assess_sicr <- function(facts, policy=NULL) {
  x <- assess_stage(facts, policy)
  list(instrument_id=x$instrument_id, sicr=identical(x$stage, "STAGE_2"),
       stage=x$stage, reason=x$stage_reason)
}

#' @rdname classification_decisions
#' @export
stage_movements <- function(stage_results) {
  if (!is.list(stage_results)) .fae_abort("stage_results must be a list of rows")
  keys <- vapply(stage_results, function(x) paste(x$prior_stage, x$stage, sep="\r"), character(1))
  counts <- table(keys)
  lapply(sort(names(counts)), function(key) {
    parts <- strsplit(key, "\r", fixed=TRUE)[[1L]]
    list(prior_stage=parts[[1L]], stage=parts[[2L]], instruments=unname(as.integer(counts[[key]])))
  })
}

#' @rdname classification_decisions
#' @export
assess_ibor_relief <- function(applicable, policy_reference, approval_reference) {
  .fae_flag(applicable, "applicable")
  if (!is.character(policy_reference) || length(policy_reference) != 1L ||
      !nzchar(trimws(policy_reference)) || !is.character(approval_reference) ||
      length(approval_reference) != 1L || !nzchar(trimws(approval_reference))) {
    .fae_abort("Explicit applicability, policy and approval are required")
  }
  list(applicable=applicable, policy_reference=policy_reference,
       approval_reference=approval_reference, status="CONTROLLED_INTERFACE",
       source_reference="IFRS9.6.8-6.9_5.4.5-5.4.9")
}
