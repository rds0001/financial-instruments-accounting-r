.fae_named_index <- function(rows, key="instrument_id") {
  ids <- vapply(rows, function(x) as.character(x[[key]]), character(1))
  if (anyDuplicated(ids)) .fae_abort(sprintf("Duplicate %s", key))
  setNames(rows, ids)
}

.fae_journal_pair <- function(batch, instrument, first_account, second_account,
                              debit, credit, movement) {
  list(
    list(journal_batch_id=batch, instrument_id=instrument, account=first_account,
         debit=debit, credit=credit, movement_type=movement),
    list(journal_batch_id=batch, instrument_id=instrument, account=second_account,
         debit=credit, credit=debit, movement_type=movement)
  )
}

.fae_backtest <- function(observations) {
  required <- c("model_type","model_version_id","predicted_value","actual_value",
                "model_owner","validation_status","approval_id")
  for (row in observations) {
    .fae_required(row, required, "backtesting observation")
    if (!row$validation_status %in% c("VALIDATED","CONDITIONALLY_VALIDATED"))
      .fae_abort("Unusable model validation status")
    if (.fae_number(row$exposure_weight, "exposure_weight", 0) == 0)
      .fae_abort("Observation weights must be positive")
  }
  if (!length(observations)) return(list())
  keys <- vapply(observations, function(x) paste(x$model_type,x$model_version_id,
    isTRUE(x$challenger_flag),x$segment %||% "ALL",sep="\r"),character(1))
  groups <- split(observations, keys)
  unname(lapply(groups[order(names(groups))], function(rows) {
    weight <- vapply(rows,function(x) as.numeric(x$exposure_weight),numeric(1))
    pred <- vapply(rows,function(x) .fae_number(x$predicted_value,"predicted_value"),numeric(1))
    actual <- vapply(rows,function(x) .fae_number(x$actual_value,"actual_value"),numeric(1))
    wm <- function(x) sum(x*weight)/sum(weight)
    auc <- NA_real_
    if (rows[[1]]$model_type %in% c("PD","STAGING")) {
      positive <- which(vapply(rows,function(x) isTRUE(x$outcome_flag),logical(1)))
      negative <- which(!vapply(rows,function(x) isTRUE(x$outcome_flag),logical(1)))
      if (length(positive) && length(negative)) {
        scores <- outer(pred[positive],pred[negative],function(a,b) ifelse(a>b,1,ifelse(a==b,.5,0)))
        auc <- mean(scores)
      }
    }
    predicted <- wm(pred); observed <- wm(actual)
    list(model_type=rows[[1]]$model_type, model_version_id=rows[[1]]$model_version_id,
      challenger_flag=isTRUE(rows[[1]]$challenger_flag), segment=rows[[1]]$segment %||% "ALL",
      observations=length(rows), weighted_prediction=predicted, weighted_actual=observed,
      calibration_ratio=if (predicted==0) NA_real_ else observed/predicted,
      mean_absolute_error=wm(abs(pred-actual)), bias=predicted-observed, auc=auc,
      release_amount=sum(vapply(rows,function(x) .fae_number(x$release_amount %||% 0,
        "release_amount"),numeric(1))), model_owner=rows[[1]]$model_owner,
      validation_status=rows[[1]]$validation_status, approval_id=rows[[1]]$approval_id,
      formula_id="F-BT-001")
  }))
}

#' Journals, Rollforwards, Disclosures and Model Monitoring
#'
#' Build balanced reference journals, reconcile balances, produce allowance and
#' gross-carrying-amount rollforwards, create atomic and aggregated disclosure
#' facts, monitor model observations, compare challengers, analyse scenario
#' sensitivity and keep regulatory expected loss separate from accounting ECL.
#'
#' @param ecl_results List of instrument ECL summary rows.
#' @param opening_allowances,opening Named numeric opening balances by instrument.
#' @param classifications,stages Named character mappings by instrument.
#' @param instruments List of canonical instrument rows.
#' @param processed_events,own_credit_results,hedge_results Lists of processed
#'   event, own-credit or hedge rows.
#' @param fx Named positive presentation-currency FX rates by instrument.
#' @param expected,actual Scalars in the same unit.
#' @param tolerance Non-negative absolute reconciliation tolerance.
#' @param presentation_currency Explicit presentation-currency code.
#' @param facts Atomic disclosure rows produced by `disclosure_facts()`.
#' @param observations Approved model-monitoring rows with positive exposure
#'   weights and explicit owner, validation status and approval ID.
#' @param losses Named non-negative standalone scenario losses.
#' @param probabilities Named scenario probabilities summing to one.
#' @param reference,challenger Monitoring result rows keyed by model type and
#'   segment.
#'
#' @return Table functions return lists of named rows. `reconcile_ledger()`
#'   returns expected, actual, signed difference, tolerance and PASS/FAIL.
#'   Journals contain batch, instrument, account, debit, credit and movement.
#'   Backtests contain weighted prediction/actual, calibration ratio, mean
#'   absolute error, bias and, where identifiable, pairwise AUC.
#'
#' @details Journals are balanced reference entries and never post to a ledger.
#'   Institutional account mapping and approval remain external. Disclosure
#'   coverage is aggregate allowance divided by aggregate gross amount.
#'   Regulatory inputs are only displayed in a separate bridge and never feed
#'   the accounting estimate. Model diagnostics and challenger comparisons are
#'   management analyses without accounting effects.
#'
#' @references International Accounting Standards Board (2014), IFRS 9,
#'   paragraphs 5.5 and 6.5; IFRS 7 paragraphs 35A--35N. Official source index:
#'   <https://www.ifrs.org/issued-standards/list-of-standards/ifrs-7-financial-instruments-disclosures/>.
#'
#' @seealso [scenario_ecl()], [risk_event_hedge], [run_dataset()]
#'
#' @examples
#' instruments <- list(list(instrument_id="x", gross_amount=100, entity_id="E",
#'                    portfolio="P", currency="EUR", regulatory_expected_loss=15))
#' ecl <- list(list(instrument_id="x", stage="STAGE_1", final_ecl=20))
#' build_journals(ecl, c(x=5), c(x="AMORTISED_COST"), instruments)
#' allowance_rollforward(instruments, c(x=5), ecl, list(), c(x=1))
#' gross_rollforward(instruments, c(x=90), list(), c(x=1))
#' reconcile_ledger(100, 99)
#' facts <- disclosure_facts(instruments, ecl, c(x="AMORTISED_COST"),
#'                           c(x="STAGE_1"), c(x=1), "EUR")
#' aggregate_disclosures(facts)
#' obs <- list(list(model_type="PD", model_version_id="v1", predicted_value=.2,
#'             actual_value=.1, exposure_weight=1, outcome_flag=FALSE,
#'             model_owner="Risk", validation_status="VALIDATED", approval_id="A",
#'             segment="X"))
#' backtest_pd(obs)
#' backtest_lgd(modifyList(obs[[1]], list(model_type="LGD")) |> list())
#' backtest_ead(modifyList(obs[[1]], list(model_type="EAD")) |> list())
#' backtest_staging(modifyList(obs[[1]], list(model_type="STAGING")) |> list())
#' backtest_models(obs)
#' scenario_sensitivity(c(base=10,stress=30),c(base=.75,stress=.25))
#' compare_challenger(list(list(model_type="PD",segment="X",mean_absolute_error=.1,bias=.02)),
#'                    list(list(model_type="PD",segment="X",mean_absolute_error=.05,bias=.01)))
#' regulatory_bridge(instruments,ecl,c(x="STAGE_1"),c(x=1))
#' @name reporting_monitoring
NULL

#' @rdname reporting_monitoring
#' @export
build_journals <- function(ecl_results, opening_allowances, classifications, instruments,
                           processed_events=list(), own_credit_results=list(), hedge_results=list()) {
  inst <- .fae_named_index(instruments); result <- list()
  for (row in ecl_results) {
    id <- as.character(row$instrument_id)
    movement <- .fae_number(row$final_ecl,"final_ecl") -
      .fae_number(opening_allowances[[id]] %||% 0,"opening_allowance")
    if (abs(movement)<1e-12) next
    account <- if (identical(classifications[[id]],"FVOCI_DEBT"))
      "FVOCI_ACCUMULATED_IMPAIRMENT_OCI" else if ((inst[[id]]$instrument_type %||% "") %in%
      c("FINANCIAL_GUARANTEE","LOAN_COMMITMENT")) "ECL_PROVISION_OFF_BALANCE" else "LOSS_ALLOWANCE"
    result <- c(result,.fae_journal_pair(paste0("ECL-",id),id,"ECL_EXPENSE",account,
      max(movement,0),max(-movement,0),"ECL_REMEASUREMENT"))
  }
  for (row in processed_events) {
    batch <- paste0("EVT-",row$event_id); amount <- abs(.fae_number(row$amount %||% 0,"amount"))
    if (row$event_type=="WRITEOFF" && amount>0) result <- c(result,
      .fae_journal_pair(batch,row$instrument_id,"LOSS_ALLOWANCE","GROSS_CARRYING_AMOUNT",
                        amount,0,"WRITEOFF"))
    else if (row$event_type=="RECOVERY" && amount>0) result <- c(result,
      .fae_journal_pair(batch,row$instrument_id,"CASH","RECOVERY_INCOME",amount,0,"RECOVERY"))
    else if (row$event_type=="ASSET_MODIFICATION_NO_DERECOGNITION" &&
             abs(row$modification_result)>1e-12) {
      v <- row$modification_result; result <- c(result,.fae_journal_pair(batch,row$instrument_id,
        "GROSS_CARRYING_AMOUNT","MODIFICATION_PNL",max(v,0),max(-v,0),"MODIFICATION"))
    } else if (row$event_type %in% c("ASSET_MODIFICATION_DERECOGNITION",
      "LIABILITY_MODIFICATION_DERECOGNITION","LIABILITY_MODIFICATION_NO_DERECOGNITION") &&
      abs(row$modification_result)>1e-12) {
      v <- row$pnl_effect; result <- c(result,.fae_journal_pair(batch,row$instrument_id,
        "MODIFICATION_OR_DERECOGNITION_PNL","FINANCIAL_INSTRUMENT_CARRYING_AMOUNT",
        max(-v,0),max(v,0),"MODIFICATION_DERECOGNITION"))
    } else if (row$event_type=="ASSET_TRANSFER" && row$derecognition_result!="NO_DERECOGNITION") {
      v <- max(amount-(row$continuing_involvement %||% 0),0)
      if (v>0) result <- c(result,.fae_journal_pair(batch,row$instrument_id,
        "TRANSFER_CONSIDERATION_RECEIVABLE","GROSS_CARRYING_AMOUNT",v,0,"ASSET_DERECOGNITION"))
    }
  }
  for (row in own_credit_results) if (identical(classifications[[as.character(row$instrument_id)]],
                                                 "FVTPL_LIABILITY")) {
    own <- row$own_credit_oci; pnl <- row$other_fv_change_pnl; total <- own+pnl
    if (abs(own)+abs(pnl)>=1e-12) {
      batch <- paste0("OWNCR-",row$instrument_id)
      result <- c(result,list(
        list(journal_batch_id=batch,instrument_id=row$instrument_id,account="FVTPL_LIABILITY",
             debit=max(-total,0),credit=max(total,0),movement_type="FAIR_VALUE_CHANGE"),
        list(journal_batch_id=batch,instrument_id=row$instrument_id,account="OWN_CREDIT_OCI",
             debit=max(own,0),credit=max(-own,0),movement_type="OWN_CREDIT"),
        list(journal_batch_id=batch,instrument_id=row$instrument_id,account="FV_CHANGE_PNL",
             debit=max(pnl,0),credit=max(-pnl,0),movement_type="FAIR_VALUE_CHANGE")))
    }
  }
  for (row in hedge_results) for (item in list(
    c("INEFF","HEDGE_INEFFECTIVENESS_PNL",row$ineffectiveness_pnl %||% 0),
    c("OCI","HEDGE_RESERVE_OCI",row$oci_effective %||% 0),
    c("COH","COST_OF_HEDGING_OCI",row$cost_of_hedging_reserve %||% 0),
    c("RECYCLE","HEDGE_RESERVE_OCI",-(row$recycle_amount %||% 0)),
    c("BASIS","HEDGE_RESERVE_OCI",-(row$basis_adjustment %||% 0)))) {
      value <- as.numeric(item[[3]]); if (abs(value)<1e-12) next
      suffix <- item[[1]]; offset <- if (suffix=="RECYCLE") "HEDGE_RECYCLING_PNL" else
        if (suffix=="BASIS") "NONFINANCIAL_ASSET_BASIS" else "HEDGE_VALUATION_OFFSET"
      result <- c(result,.fae_journal_pair(paste("HDG",row$relationship_id,suffix,sep="-"),
        row$relationship_id,item[[2]],offset,max(-value,0),max(value,0),"HEDGE_ACCOUNTING"))
  }
  for (row in result) { .fae_number(row$debit,"debit",0); .fae_number(row$credit,"credit",0) }
  for (batch in unique(vapply(result,function(x)x$journal_batch_id,character(1)))) {
    rows <- Filter(function(x) x$journal_batch_id==batch,result)
    if (abs(sum(vapply(rows,function(x)x$debit-x$credit,numeric(1))))>1e-9)
      .fae_abort("Unbalanced journal batch")
  }
  result
}

#' @rdname reporting_monitoring
#' @export
allowance_rollforward <- function(instruments, opening, ecl_results, processed_events, fx) {
  ecl <- .fae_named_index(ecl_results)
  lapply(instruments,function(inst) {
    id <- as.character(inst$instrument_id); opened <- opening[[id]] %||% 0
    target <- ecl[[id]]$final_ecl
    events <- Filter(function(x) x$instrument_id==id && x$event_type=="WRITEOFF",processed_events)
    writeoff <- sum(vapply(events,function(x)x$amount*fx[[id]],numeric(1)))
    remeasurement <- target-opened; closing <- opened+remeasurement-writeoff
    if (!isTRUE(inst$poci)) closing <- max(closing,0)
    list(instrument_id=id,prior_stage=ecl[[id]]$prior_stage,current_stage=ecl[[id]]$stage,
      opening_allowance=opened,ecl_remeasurement=remeasurement,writeoff=writeoff,
      closing_allowance=closing,formula_id="F-ROLL-001")
  })
}

#' @rdname reporting_monitoring
#' @export
gross_rollforward <- function(instruments, opening, processed_events, fx) {
  result <- list()
  for (inst in instruments) if (!isTRUE(inst$liability)) {
    id <- as.character(inst$instrument_id); opened <- opening[[id]] %||% 0
    events <- Filter(function(x)x$instrument_id==id,processed_events)
    writeoff <- sum(vapply(Filter(function(x)x$event_type=="WRITEOFF",events),
      function(x)x$amount*fx[[id]],numeric(1)))
    derec <- sum(vapply(Filter(function(x)x$event_type=="ASSET_TRANSFER" &&
      x$derecognition_result!="NO_DERECOGNITION",events),function(x)
        max(x$amount-(x$continuing_involvement %||% 0),0)*fx[[id]],numeric(1)))
    modification <- sum(vapply(Filter(function(x)startsWith(x$event_type,"ASSET_MODIFICATION"),events),
      function(x)(x$modification_result %||% 0)*fx[[id]],numeric(1)))
    current <- (inst$gross_amount %||% 0)*fx[[id]]; other <- current-opened-modification
    result[[length(result)+1L]] <- list(instrument_id=id,opening_gross=opened,
      modification=modification,other_contractual_and_fx_movements=other,writeoff=writeoff,
      derecognition=derec,closing_gross=opened+modification+other-writeoff-derec,
      formula_id="F-ROLL-001")
  }
  result
}

#' @rdname reporting_monitoring
#' @export
reconcile_ledger <- function(expected, actual, tolerance=1e-9) {
  expected <- .fae_number(expected,"expected"); actual <- .fae_number(actual,"actual")
  tolerance <- .fae_number(tolerance,"tolerance",0); difference <- actual-expected
  list(expected=expected,actual=actual,difference=difference,tolerance=tolerance,
       status=if (abs(difference)<=tolerance) "PASS" else "FAIL")
}

#' @rdname reporting_monitoring
#' @export
disclosure_facts <- function(instruments,ecl_results,classifications,stages,fx,presentation_currency) {
  ecl <- .fae_named_index(ecl_results)
  lapply(instruments,function(inst) {
    id <- as.character(inst$instrument_id); rate <- .fae_number(fx[[id]],"fx",0)
    gross <- .fae_number(inst$gross_amount,"gross_amount",0)*rate
    allowance <- .fae_number(ecl[[id]]$final_ecl,"final_ecl")
    list(instrument_id=id,entity_id=inst$entity_id,portfolio=inst$portfolio,
      classification=classifications[[id]],stage=stages[[id]],gross_carrying_amount=gross,
      allowance=allowance,net_carrying_amount=gross-allowance,currency=inst$currency,
      presentation_currency=presentation_currency,fx_rate=rate)
  })
}

#' @rdname reporting_monitoring
#' @export
aggregate_disclosures <- function(facts) {
  currencies <- unique(vapply(facts,function(x)as.character(x$presentation_currency),character(1)))
  if (length(currencies)>1L) .fae_abort("Mixed presentation currencies")
  dimensions <- list("entity_id","portfolio","classification","stage","currency",
                     c("entity_id","portfolio","classification","stage","currency"))
  result <- list()
  for (fields in dimensions) {
    keys <- vapply(facts,function(x)paste(vapply(fields,function(f)as.character(x[[f]]),character(1)),
                                         collapse="|"),character(1))
    groups <- split(facts,keys)
    for (key in sort(names(groups))) {
      rows <- groups[[key]]; gross <- sum(vapply(rows,function(x)x$gross_carrying_amount,numeric(1)))
      allowance <- sum(vapply(rows,function(x)x$allowance,numeric(1)))
      result[[length(result)+1L]] <- list(aggregation_level=paste(fields,collapse="+"),
        dimension_key=key,instrument_count=length(rows),gross_carrying_amount=gross,
        allowance=allowance,net_carrying_amount=gross-allowance,
        coverage_ratio=if (gross==0) NA_real_ else allowance/gross,formula_id="F-AGG-001")
    }
  }
  result
}

#' @rdname reporting_monitoring
#' @export
backtest_pd <- function(observations) .fae_backtest(Filter(function(x)x$model_type=="PD",observations))
#' @rdname reporting_monitoring
#' @export
backtest_lgd <- function(observations) .fae_backtest(Filter(function(x)x$model_type=="LGD",observations))
#' @rdname reporting_monitoring
#' @export
backtest_ead <- function(observations) .fae_backtest(Filter(function(x)x$model_type=="EAD",observations))
#' @rdname reporting_monitoring
#' @export
backtest_staging <- function(observations) .fae_backtest(Filter(function(x)x$model_type=="STAGING",observations))

#' @rdname reporting_monitoring
#' @export
backtest_models <- function(observations) {
  allowed <- c("PD","LGD","EAD","CCF","STAGING","SCENARIO","OVERLAY")
  if (any(!vapply(observations,function(x)x$model_type %in% allowed,logical(1))))
    .fae_abort("Unknown monitoring model type")
  .fae_backtest(observations)
}

#' @rdname reporting_monitoring
#' @export
scenario_sensitivity <- function(losses,probabilities) {
  probabilities <- .fae_weights(probabilities)
  if (!setequal(names(losses),names(probabilities)) || length(losses)!=length(probabilities))
    .fae_abort("Scenario keys differ")
  lapply(names(probabilities),function(key) {
    loss <- .fae_number(losses[[key]],key,0); p <- probabilities[[key]]
    list(scenario_id=key,standalone_ecl=loss,probability=p,weighted_contribution=loss*p,
         view="MANAGEMENT")
  })
}

#' @rdname reporting_monitoring
#' @export
compare_challenger <- function(reference,challenger) {
  keys <- vapply(reference,function(x)paste(x$model_type,x$segment,sep="|"),character(1))
  if (anyDuplicated(keys)) .fae_abort("Ambiguous reference model/segment")
  index <- setNames(reference,keys)
  lapply(challenger,function(row) {
    key <- paste(row$model_type,row$segment,sep="|")
    if (is.null(index[[key]])) .fae_abort("Missing reference comparison group")
    list(model_type=row$model_type,segment=row$segment,
      mae_difference=.fae_number(row$mean_absolute_error,"MAE")-
        .fae_number(index[[key]]$mean_absolute_error,"MAE"),
      bias_difference=.fae_number(row$bias,"bias")-.fae_number(index[[key]]$bias,"bias"),
      view="MANAGEMENT")
  })
}

#' @rdname reporting_monitoring
#' @export
regulatory_bridge <- function(instruments,ecl_results,stages,fx) {
  ecl <- .fae_named_index(ecl_results)
  lapply(instruments,function(inst) {
    id <- as.character(inst$instrument_id); accounting <- ecl[[id]]$final_ecl
    regulatory <- (inst$regulatory_expected_loss %||% 0)*fx[[id]]
    list(instrument_id=id,view="REGULATORY_BRIDGE",regulatory_approach=inst$regulatory_approach,
      ifrs9_stage=stages[[id]],regulatory_default_flag=isTRUE(inst$regulatory_default_flag),
      ifrs9_ecl=accounting,regulatory_expected_loss=regulatory,difference=accounting-regulatory,
      accounting_value_used_in_regulatory_calculation=FALSE,formula_id="F-REG-BRIDGE-001")
  })
}
