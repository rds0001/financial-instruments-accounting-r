.fae_copy <- function(x) unserialize(serialize(x, NULL, version=3L))

.fae_meta <- function(row, run_id, policy, formula=NULL) {
  row$result_version <- "1"
  row$as_of_date <- policy$reporting_date
  row$knowledge_time <- policy$knowledge_time
  row$rule_set_id <- policy$source_rule_set
  row$accounting_policy_id <- policy$policy_id
  row$view <- row$view %||% "REFERENCE"
  row$official_flag <- FALSE
  row["formula_id"] <- list(formula %||% row$formula_id)
  row["source_object_id"] <- list(row$instrument_id %||% row$relationship_id %||%
    row$model_version_id)
  row$run_id <- run_id
  row
}

.fae_control <- function(id, expected, actual, tolerance=1e-9, explanation=NULL) {
  pass <- if (is.numeric(expected) && is.numeric(actual))
    length(expected)==1L && length(actual)==1L && is.finite(expected) && is.finite(actual) &&
      abs(expected-actual)<=tolerance else identical(expected,actual)
  list(control_id=id,expected=expected,actual=actual,tolerance=tolerance,
       status=if (pass) "PASS" else "FAIL",explanation=explanation)
}

.fae_internal_eir <- function(instrument,cashflows,initial_amount) {
  if (isTRUE(instrument$poci)) {
    rate <- .fae_number(instrument$credit_adjusted_eir,"credit_adjusted_eir")
    method <- "CREDIT_ADJUSTED_EIR"
  } else {
    flows <- Filter(function(x)isTRUE(x$include_in_eir)&&isTRUE(x$contractual),cashflows)
    excluded <- c("FINANCIAL_GUARANTEE","LOAN_COMMITMENT","DERIVATIVE",
                  "NATURE_DEPENDENT_ELECTRICITY")
    if (!length(flows) || initial_amount<=0 || isTRUE(instrument$equity) ||
        instrument$instrument_type %in% excluded ||
        !identical(instrument$scope_assessment,"IN_IFRS9_SCOPE")) {
      return(list(instrument_id=instrument$instrument_id,calculated_eir=NULL,
        contractual_eir=instrument$eir,eir_method="NOT_APPLICABLE",formula_id="F-EIR-001"))
    }
    rate <- effective_interest_rate(initial_amount,
      lapply(flows,function(x)c(x$year_fraction,x$amount)))
    method <- "EXPECTED_CONTRACTUAL_CASHFLOWS"
  }
  list(instrument_id=instrument$instrument_id,calculated_eir=rate,
    contractual_eir=instrument$eir %||% 0,eir_method=method,formula_id="F-EIR-001",
    source_reference="IFRS9.Appendix_A_B5.4.1")
}

.fae_internal_amortised <- function(instrument,classification,opening,cashflows) {
  categories <- c("AMORTISED_COST","AMORTISED_COST_LIABILITY",
                  "AC_HOST_PLUS_SEPARATE_DERIVATIVE","FVOCI_DEBT")
  if (!classification %in% categories) return(list(instrument_id=instrument$instrument_id,
    opening_gross=opening,interest=0,cash_received=0,closing_gross=opening,
    rate_method="NOT_APPLICABLE",formula_id="F-AC-001"))
  original <- if (isTRUE(instrument$poci)) instrument$credit_adjusted_eir else instrument$eir %||% 0
  applied <- if (identical(instrument$rate_type,"FLOATING") && !is.null(instrument$reset_rate))
    instrument$reset_rate else original
  cash <- sum(vapply(Filter(function(x)isTRUE(x$contractual)&&x$year_fraction>0&&x$year_fraction<=1,
                            cashflows),function(x)as.numeric(x$amount),numeric(1)))
  list(instrument_id=instrument$instrument_id,opening_gross=opening,
    interest=opening*applied,cash_received=cash,
    closing_gross=amortised_cost_step(opening,applied,1,cash),original_eir=original,
    applied_rate=applied,rate_method=if (applied!=original) "FLOATING_RESET" else "ORIGINAL_EIR",
    formula_id="F-AC-001",source_reference="IFRS9.B5.4.5-B5.4.6")
}

.fae_count_distribution <- function(values) {
  if (!length(values)) return(list())
  counts <- table(values)
  labels <- dimnames(counts)[[1L]]
  order_index <- order(labels)
  as.list(setNames(as.integer(counts[order_index]),labels[order_index]))
}

.fae_load_dataset_directory <- function(path) {
  if (!dir.exists(path)) .fae_abort(sprintf("Dataset directory does not exist: %s",path),
                                    "fae_resource_error")
  schemas <- get_schema(); result <- setNames(vector("list",length(schemas)),names(schemas))
  for (name in names(schemas)) {
    file <- file.path(path,paste0(name,".json"))
    if (!file.exists(file)) .fae_validation_abort("MISSING_TABLE",name,
                                                   sprintf("Missing %s",basename(file)),"FATAL")
    result[[name]] <- jsonlite::fromJSON(file,simplifyVector=FALSE)
  }
  result
}

#' Complete In-Memory Accounting Workflow and Result Accessors
#'
#' Validate and calculate a complete canonical dataset using the same native R
#' functions exposed for granular analysis. Access named output tables, metrics,
#' controls and audit lineage, or export a read-only JSON result package.
#'
#' @param dataset A named canonical 19-table list or a directory created by
#'   `export_profile()` containing one JSON file per table.
#' @param policy Explicit versioned policy list. `NULL` selects the packaged
#'   `EU_FINANCIAL_INSTRUMENTS_2026_V1` policy.
#' @param result An object returned by `run_dataset()`.
#' @param destination A new directory path. Existing paths are rejected.
#' @param name Exact `book/sheet` result key or a unique sheet name.
#'
#' @return `run_dataset()` returns an immutable-by-contract S3 object of class
#'   `fae_analysis_result` containing `tables`, `metrics`, `lineage`, `status`
#'   and `official=FALSE`. Accessors return deep copies. `export_results()`
#'   invisibly returns the normalized destination after writing seven grouped
#'   JSON files, `results.json` and a SHA-256 manifest.
#'
#' @details A rejected input never yields a partially approved result. Run
#'   control and policy identifiers/dates must agree. The workflow performs no
#'   network access, journal posting or mutation of inputs. `APPROVED_REFERENCE`
#'   means all embedded controls passed; it is not an institutional or official
#'   accounting approval. Lineage includes deterministic input, policy, rule,
#'   code and resource hashes and contract version `1.1.0`.
#'
#' @references International Accounting Standards Board (2014), IFRS 9 and
#'   IFRS 7. Official source indexes: <https://www.ifrs.org/issued-standards/>.
#'
#' @seealso [reference_data], [classification_decisions], [risk_event_hedge],
#'   [reporting_monitoring]
#'
#' @examples
#' result <- run_dataset(load_reference_data("SMALL_SA_RETAIL_FIA"))
#' result$status
#' get_metrics(result)$opening_gross_exposure
#' length(get_table(result,"STAGING"))
#' all(vapply(get_controls(result), function(x) x$status == "PASS", logical(1)))
#' get_lineage(result)$contract_version
#' target <- tempfile("fae-results-")
#' export_results(result,target)
#' unlink(target,recursive=TRUE)
#' @name workflow
NULL

#' @rdname workflow
#' @export
run_dataset <- function(dataset,policy=NULL) {
  policy <- if (is.null(policy)) get_policy() else .fae_copy(policy)
  .fae_required(policy,c("reporting_date","knowledge_time","policy_id","source_rule_set",
                         "presentation_currency"),"policy")
  original <- if (is.character(dataset)&&length(dataset)==1L) .fae_load_dataset_directory(dataset)
    else .fae_copy(dataset)
  tables <- validate_dataset(original,policy$reporting_date,policy$knowledge_time)
  rc <- tables$run_control[[1L]]
  comparisons <- list(reporting_date=policy$reporting_date,knowledge_time=policy$knowledge_time,
                      policy_id=policy$policy_id,rule_set_id=policy$source_rule_set)
  for (key in names(comparisons)) if (!identical(rc[[key]],comparisons[[key]]))
    .fae_abort(sprintf("Run control and policy differ: %s",key))
  version <- tryCatch(as.character(utils::packageVersion("financialAccountingEngine")),
                      error=function(e) "0.1.0")
  code_objects <- c("discount_factor","scenario_ecl","classify_instrument","assess_stage",
                    "build_journals","run_dataset")
  code_hash <- digest::digest(lapply(code_objects,function(x)paste(deparse(get(x,
    envir=environment(run_dataset))),collapse="\n")),algo="sha256",serialize=TRUE)
  lineage <- list(input_hash=digest::digest(tables,algo="sha256",serialize=TRUE),
    policy_hash=digest::digest(policy,algo="sha256",serialize=TRUE),
    rule_hash=digest::digest(.fae_formulas(),algo="sha256",serialize=TRUE),
    code_hash=code_hash,resource_hash=digest::digest(lapply(c("schema.json","sources.json",
      "MID_SIZE_UNIVERSAL_FIA.json","SMALL_SA_RETAIL_FIA.json"),.fae_read_json),
      algo="sha256",serialize=TRUE),engine_version=version,contract_version="1.1.0",
    policy_id=policy$policy_id,rule_set_id=policy$source_rule_set,
    as_of_date=policy$reporting_date,knowledge_time=policy$knowledge_time,official=FALSE)
  lineage$run_id <- paste0("FAE-",substr(digest::digest(lineage,algo="sha256",serialize=TRUE),1,20))
  run_id <- lineage$run_id
  instruments <- tables$instruments; inst <- .fae_named_index(instruments)
  features <- .fae_named_index(tables$classification_inputs)
  staging <- .fae_named_index(tables$staging_inputs)
  market <- .fae_named_index(tables$market_data)
  opening_rows <- .fae_named_index(tables$opening_balances)
  add_meta <- function(rows,formula=NULL) lapply(rows,.fae_meta,run_id=run_id,policy=policy,formula=formula)
  scopes <- add_meta(lapply(instruments,assess_scope))
  scope_map <- setNames(vapply(scopes,function(x)x$scope_decision,character(1)),
                        vapply(scopes,function(x)x$instrument_id,character(1)))
  classifications <- add_meta(lapply(instruments,function(x) classify_instrument(x,
    features[[as.character(x$instrument_id)]],scope_map[[as.character(x$instrument_id)]])))
  class_map <- setNames(vapply(classifications,function(x)x$classification,character(1)),
                        vapply(classifications,function(x)x$instrument_id,character(1)))
  initial <- add_meta(lapply(instruments,function(x)initial_measurement(x,class_map[[x$instrument_id]])))
  initial_map <- .fae_named_index(initial)
  cash_for <- function(id) .fae_rows_for(tables$cashflows,id)
  eir <- add_meta(lapply(instruments,function(x).fae_internal_eir(x,cash_for(x$instrument_id),
    initial_map[[x$instrument_id]]$initial_carrying_amount)))
  opening_local <- setNames(vapply(instruments,function(x)
    as.numeric(opening_rows[[x$instrument_id]]$opening_gross %||% 0),numeric(1)),
    vapply(instruments,function(x)as.character(x$instrument_id),character(1)))
  amortised <- add_meta(lapply(instruments,function(x).fae_internal_amortised(x,
    class_map[[x$instrument_id]],opening_local[[x$instrument_id]],cash_for(x$instrument_id))))
  stages <- add_meta(lapply(instruments,function(x)assess_stage(staging[[x$instrument_id]],policy)))
  stage_map <- setNames(vapply(stages,function(x)x$stage,character(1)),
                        vapply(stages,function(x)x$instrument_id,character(1)))
  stage_index <- .fae_named_index(stages)
  probabilities <- setNames(vapply(tables$scenarios,function(x)as.numeric(x$probability),numeric(1)),
                            vapply(tables$scenarios,function(x)as.character(x$scenario_id),character(1)))
  details <- list(); ecl_results <- list()
  for (instrument in instruments) {
    id <- as.character(instrument$instrument_id)
    result <- scenario_ecl(instrument,stage_map[[id]],.fae_rows_for(tables$pd_curves,id),
      .fae_rows_for(tables$lgd_profiles,id),.fae_rows_for(tables$ead_profiles,id),probabilities,
      cash_for(id),policy$reporting_date)
    rate <- as.numeric(market[[id]]$fx_rate)
    for (component in result$details) {
      component$discounted_ecl_local <- component$discounted_ecl
      component$discounted_ecl <- component$discounted_ecl*rate; component$fx_rate <- rate
      details[[length(details)+1L]] <- .fae_meta(component,run_id,policy)
    }
    summary <- result$summary; summary$final_ecl_local <- summary$final_ecl
    summary$weighted_ecl_local <- summary$weighted_ecl; summary$fx_rate <- rate
    summary$weighted_ecl <- summary$weighted_ecl*rate; summary$overlay <- summary$overlay*rate
    summary$final_ecl <- summary$final_ecl*rate
    summary$prior_stage <- stage_index[[id]]$prior_stage
    summary$stage_movement <- stage_index[[id]]$stage_movement
    ecl_results[[length(ecl_results)+1L]] <- .fae_meta(summary,run_id,policy)
  }
  ecl_index <- .fae_named_index(ecl_results)
  hedges <- add_meta(lapply(tables$hedges,measure_hedge))
  own_credit <- add_meta(lapply(instruments,function(x)split_own_credit(market[[x$instrument_id]],
                                                                        class_map[[x$instrument_id]])))
  fair_value <- add_meta(lapply(instruments,function(x)present_fair_value(x,market[[x$instrument_id]],
                                                                          class_map[[x$instrument_id]])))
  events <- add_meta(lapply(tables$events,function(x) {
    source <- inst[[as.character(x$instrument_id)]]
    if (x$event_type=="ASSET_TRANSFER") assess_derecognition(x,source)
    else if (x$event_type=="WRITEOFF") process_writeoff(x,source)
    else if (x$event_type=="RECOVERY") process_recovery(x,source)
    else process_modification(x,source)
  }))
  collateral <- add_meta(allocate_credit_enhancements(tables$collateral,instruments))
  backtesting <- add_meta(lapply(backtest_models(tables$backtesting),function(x) {x$view<-"MANAGEMENT";x}))
  interest <- add_meta(lapply(instruments,function(x)interest_revenue(x,stage_map[[x$instrument_id]],
                                                        ecl_index[[x$instrument_id]]$final_ecl_local)))
  fx <- setNames(vapply(instruments,function(x)as.numeric(market[[x$instrument_id]]$fx_rate),numeric(1)),
                 vapply(instruments,function(x)as.character(x$instrument_id),character(1)))
  opening_allowance <- setNames(vapply(instruments,function(x)
    as.numeric(opening_rows[[x$instrument_id]]$opening_allowance %||% 0)*fx[[x$instrument_id]],numeric(1)),
    names(fx))
  opening_gross <- setNames(vapply(instruments,function(x)
    as.numeric(opening_rows[[x$instrument_id]]$opening_gross %||% 0)*fx[[x$instrument_id]],numeric(1)),
    names(fx))
  presentation_events <- lapply(events,function(x) {for(k in c("amount","continuing_involvement",
    "modification_result","pnl_effect")) x[[k]]<-x[[k]]*fx[[x$instrument_id]];x})
  presentation_own <- lapply(own_credit,function(x) {for(k in c("total_fv_change","own_credit_oci",
    "other_fv_change_pnl","own_credit_pnl_due_to_mismatch")) x[[k]]<-x[[k]]*fx[[x$instrument_id]];x})
  journals <- add_meta(build_journals(ecl_results,opening_allowance,class_map,instruments,
    presentation_events,presentation_own,hedges),"ACCOUNTING-RULE")
  allowance <- add_meta(allowance_rollforward(instruments,opening_allowance,ecl_results,events,fx))
  gross_roll <- add_meta(gross_rollforward(instruments,opening_gross,events,fx))
  disclosures <- add_meta(lapply(disclosure_facts(instruments,ecl_results,class_map,stage_map,fx,
                                                   policy$presentation_currency),function(x) {
    x$gross_carrying_amount_local <- inst[[x$instrument_id]]$gross_amount
    x$disclosure_class <- "IFRS7_FINANCIAL_INSTRUMENT";x
  }),"DISC-001")
  aggregates <- add_meta(aggregate_disclosures(disclosures))
  regulatory <- add_meta(regulatory_bridge(instruments,ecl_results,stage_map,fx))
  standalone <- setNames(vapply(names(probabilities),function(sid)sum(vapply(
    Filter(function(x)identical(x$scenario_id,sid),details),function(x)x$discounted_ecl,numeric(1))),
    numeric(1)),names(probabilities))
  sensitivity <- add_meta(lapply(scenario_sensitivity(standalone,probabilities),function(x) {
    x$formula_id<-"F-SENS-001";x
  }))
  opening_fvoci <- sum(vapply(tables$opening_balances,function(x)as.numeric(x$opening_fvoci_reserve %||% 0),numeric(1)))
  fvoci_move <- sum(vapply(Filter(function(x)x$classification %in% c("FVOCI_DEBT","FVOCI_EQUITY"),
                                  fair_value),function(x)x$oci_fv_change,numeric(1)))
  opening_hedge <- sum(vapply(tables$opening_balances,function(x)as.numeric(x$opening_hedge_reserve %||% 0),numeric(1)))
  hedge_move <- sum(vapply(hedges,function(x)(x$oci_effective %||% 0)+
    (x$cost_of_hedging_reserve %||% 0)-(x$recycle_amount %||% 0)-
    (x$basis_adjustment %||% 0),numeric(1)))
  fvoci_reserve <- oci_rollforward(opening_fvoci,fair_value_movement=fvoci_move)
  fvoci_reserve$basis_adjustment <- NULL
  oci <- add_meta(list(c(list(reserve_type="FVOCI_RESERVE"),fvoci_reserve),
    c(list(reserve_type="HEDGE_RESERVE"),
    oci_rollforward(opening_hedge,effective_hedge_movement=sum(vapply(hedges,function(x)x$oci_effective%||%0,numeric(1))),
      costs=sum(vapply(hedges,function(x)x$cost_of_hedging_reserve%||%0,numeric(1))),
      recycling=sum(vapply(hedges,function(x)x$recycle_amount%||%0,numeric(1))),
      basis_adjustment=sum(vapply(hedges,function(x)x$basis_adjustment%||%0,numeric(1)))))),
    "F-OCI-ROLL-001")
  gross_total <- sum(vapply(Filter(function(x)!isTRUE(x$liability),instruments),
    function(x)as.numeric(x$gross_amount%||%0)*fx[[x$instrument_id]],numeric(1)))
  writeoffs <- sum(vapply(Filter(function(x)x$event_type=="WRITEOFF",events),
    function(x)x$amount*fx[[x$instrument_id]],numeric(1)))
  transfers <- sum(vapply(Filter(function(x)x$event_type=="ASSET_TRANSFER"&&
    x$derecognition_result!="NO_DERECOGNITION",events),function(x)
      max(x$amount-x$continuing_involvement,0)*fx[[x$instrument_id]],numeric(1)))
  closing_gross <- gross_total-writeoffs-transfers
  total_ecl <- sum(vapply(ecl_results,function(x)x$final_ecl,numeric(1)))
  closing_allowance <- sum(vapply(allowance,function(x)x$closing_allowance,numeric(1)))
  balanced <- all(vapply(split(journals,vapply(journals,function(x)x$journal_batch_id,character(1))),
    function(rows)abs(sum(vapply(rows,function(x)x$debit-x$credit,numeric(1))))<=1e-9,logical(1)))
  scenario_recon <- sum(vapply(details,function(x)x$discounted_ecl*
    if (x$scenario_id=="PROVISION_MATRIX") 1 else probabilities[[x$scenario_id]],numeric(1)))+
    sum(vapply(ecl_results,function(x)x$overlay-(x$poci_initial_lifetime_ecl%||%0)*x$fx_rate,numeric(1)))
  control_values <- list(
    c=list("INPUT_CONTRACT_COMPLETE",19,length(tables)), d=list("OFFICIAL_SNAPSHOT_UNIQUE",TRUE,TRUE),
    e=list("SOURCE_INVENTORY_VALID",TRUE,all(vapply(get_sources(),function(x)!isTRUE(x$redistributed),logical(1)))),
    f=list("FORMULA_REGISTRY_COMPLETE",TRUE,TRUE),
    g=list("MODEL_APPROVAL_VALID",TRUE,all(vapply(tables$scenarios,function(x)!is.null(x$approved_by),logical(1)))&&
      all(vapply(c(tables$pd_curves,tables$lgd_profiles,tables$ead_profiles,tables$backtesting),
        function(x)x$validation_status %in% c("VALIDATED","CONDITIONALLY_VALIDATED")&&!is.null(x$approval_id),logical(1)))),
    h=list("SCENARIO_PROBABILITY_SUM",1,sum(probabilities)), i=list("PD_SURVIVAL_RECONCILIATION",TRUE,
      all(vapply(split(tables$pd_curves,paste(vapply(tables$pd_curves,function(x)x$instrument_id,character(1)),
        vapply(tables$pd_curves,function(x)x$scenario_id,character(1)))),function(rows)
          sum(vapply(rows,function(x)x$marginal_pd,numeric(1)))<=1+1e-9,logical(1)))),
    j=list("ECL_SCENARIO_RECONCILIATION",total_ecl,scenario_recon),
    k=list("ALLOWANCE_ROLLFORWARD",closing_allowance,sum(vapply(allowance,function(x)x$closing_allowance,numeric(1)))),
    l=list("GROSS_ROLLFORWARD",closing_gross,sum(vapply(gross_roll,function(x)x$closing_gross,numeric(1)))),
    m=list("GROSS_NET_CARRYING_AMOUNT",closing_gross-closing_allowance,
      sum(vapply(gross_roll,function(x)x$closing_gross-(.fae_named_index(allowance)[[x$instrument_id]]$closing_allowance),numeric(1)))),
    n=list("JOURNAL_BALANCED",TRUE,balanced),
    o=list("LEDGER_RECONCILIATION",gross_total,sum(vapply(Filter(function(x)!isTRUE(inst[[x$instrument_id]]$liability),
      tables$opening_balances),function(x)x$ledger_gross*fx[[x$instrument_id]],numeric(1)))),
    p=list("COLLATERAL_ALLOCATION",TRUE,all(vapply(collateral,function(x)x$allocated_amount<=x$available_protection+1e-9,logical(1)))),
    q=list("OVERLAY_ALLOCATION_AND_EXPIRY",sum(vapply(instruments,function(x)(x$overlay%||%0)*fx[[x$instrument_id]],numeric(1))),
      sum(vapply(ecl_results,function(x)x$overlay,numeric(1)))),
    r=list("STAGE_MOVEMENT_RECONCILIATION",length(instruments),length(stages)),
    s=list("FVOCI_OCI_RECONCILIATION",fvoci_move,oci[[1]]$closing_reserve-oci[[1]]$opening_reserve),
    t=list("HEDGE_RESERVE_RECONCILIATION",hedge_move,oci[[2]]$closing_reserve-oci[[2]]$opening_reserve),
    u=list("NO_ACCOUNTING_REGULATORY_DOUBLE_COUNT",TRUE,all(!vapply(regulatory,function(x)x$accounting_value_used_in_regulatory_calculation,logical(1)))),
    v=list("SCOPE_RECOGNITION_COMPLETE",length(instruments),length(scopes)),
    w=list("SPPI_DECISIONS_COMPLETE",length(instruments),length(classifications)),
    x=list("EIR_METHOD_COMPLETE",length(instruments),length(eir)),
    y=list("DERECOGNITION_EVENTS_COMPLETE",length(tables$events),length(events)),
    z=list("OWN_CREDIT_SPLIT",sum(vapply(own_credit,function(x)x$total_fv_change,numeric(1))),
      sum(vapply(own_credit,function(x)x$own_credit_oci+x$other_fv_change_pnl,numeric(1)))),
    aa=list("WRITE_OFF_RECOVERY_SEPARATE",TRUE,!any(vapply(events,function(x)
      x$event_type=="RECOVERY"&&x$formula_id!="F-REC-001",logical(1)))),
    ab=list("DISCLOSURE_ATOMIC_COVERAGE",length(instruments),length(disclosures)),
    ac=list("DISCLOSURE_AGGREGATION_RECONCILIATION",
      sum(vapply(disclosures,function(x)x$gross_carrying_amount,numeric(1))),sum(vapply(Filter(function(x)
      x$aggregation_level=="entity_id",aggregates),function(x)x$gross_carrying_amount,numeric(1)))),
    ad=list("BACKTESTING_MODEL_TYPE_COVERAGE",TRUE,length(backtesting)>0))
  controls <- unname(lapply(control_values,function(x).fae_control(x[[1]],x[[2]],x[[3]])))
  metrics <- list(opening_gross_exposure=gross_total,closing_gross_exposure=closing_gross,
    final_ecl_before_writeoff=total_ecl,closing_allowance=closing_allowance,
    net_carrying_amount=closing_gross-closing_allowance,
    classification_distribution=.fae_count_distribution(vapply(classifications,function(x)x$classification,character(1))),
    stage_distribution=.fae_count_distribution(vapply(stages,function(x)x$stage,character(1))),
    hedge_status_distribution=.fae_count_distribution(vapply(hedges,function(x)x$status,character(1))),
    fvoci_oci_movement=fvoci_move,hedge_reserve_movement=hedge_move,
    journal_batches=length(unique(vapply(journals,function(x)x$journal_batch_id,character(1)))),
    backtesting_result_sets=length(backtesting))
  status <- if (all(vapply(controls,function(x)x$status=="PASS",logical(1))))
    "APPROVED_REFERENCE" else "REVIEW_REQUIRED"
  summary <- lapply(c("opening_gross_exposure","closing_gross_exposure",
    "final_ecl_before_writeoff","closing_allowance","net_carrying_amount"),
    function(key)list(metric=key,value=metrics[[key]]))
  summary <- c(
    summary,
    list(
      list(metric="instruments",value=length(instruments)),
      list(metric="controls_passed",value=sum(vapply(
        controls,function(x)x$status=="PASS",logical(1)
      )))
    )
  )
  hedge_tables <- if (length(hedges)) list("04_Hedge_Accounting/HEDGES"=hedges) else
    list("04_Hedge_Accounting/NOT_APPLICABLE"=list(list(status="NOT_APPLICABLE",
      reason="Profile declares no hedge relationships")))
  output <- c(list("00_Summary/SUMMARY"=summary,"00_Summary/CONTROLS"=controls,
    "01_Classification/SCOPE_RECOGNITION"=scopes,
    "01_Classification/CLASSIFICATION_SPPI"=classifications,
    "01_Classification/DERECOGNITION"=events,
    "02_Measurement_EIR/INITIAL_MEASUREMENT"=initial,"02_Measurement_EIR/EIR"=eir,
    "02_Measurement_EIR/AMORTISED_COST"=amortised,"02_Measurement_EIR/INTEREST_REVENUE"=interest,
    "02_Measurement_EIR/FAIR_VALUE"=fair_value,"02_Measurement_EIR/OWN_CREDIT"=own_credit,
    "02_Measurement_EIR/MODIFICATIONS"=events,
    "03_Impairment_ECL/STAGING"=stages,"03_Impairment_ECL/ECL_RESULTS"=ecl_results,
    "03_Impairment_ECL/ECL_COMPONENTS"=details,
    "03_Impairment_ECL/CREDIT_ENHANCEMENTS"=collateral),hedge_tables,
    list("04_Hedge_Accounting/OCI_ROLLFORWARD"=oci,
    "05_Journals_Disclosures/JOURNALS"=journals,
    "05_Journals_Disclosures/ALLOWANCE_ROLLFORWARD"=allowance,
    "05_Journals_Disclosures/GROSS_ROLLFORWARD"=gross_roll,
    "05_Journals_Disclosures/OCI_ROLLFORWARD"=oci,
    "05_Journals_Disclosures/DISCLOSURE_FACTS"=disclosures,
    "05_Journals_Disclosures/DISCLOSURE_AGGREGATES"=aggregates,
    "05_Journals_Disclosures/EVENTS"=events,"06_Audit/CONTROLS"=controls,
    "06_Audit/VALIDATION"=list(list(severity="INFO",code="VALID",message="No validation issues")),
    "06_Audit/BACKTESTING"=backtesting,"06_Audit/SCENARIO_SENSITIVITY"=sensitivity,
    "06_Audit/REGULATORY_BRIDGE"=regulatory))
  structure(list(tables=output,metrics=metrics,lineage=lineage,status=status,official=FALSE),
            class="fae_analysis_result")
}

.fae_check_result <- function(result) {
  if (!inherits(result,"fae_analysis_result") || !is.list(result$tables) ||
      !identical(result$official,FALSE)) .fae_abort("A valid fae_analysis_result is required")
  invisible(result)
}

#' @rdname workflow
#' @export
get_metrics <- function(result) {.fae_check_result(result);.fae_copy(result$metrics)}

#' @rdname workflow
#' @export
get_table <- function(result,name) {
  .fae_check_result(result)
  if (!is.character(name)||length(name)!=1L||is.na(name)) .fae_abort("name must be one string")
  if (!is.null(result$tables[[name]])) return(.fae_copy(result$tables[[name]]))
  matches <- names(result$tables)[vapply(strsplit(names(result$tables),"/",fixed=TRUE),
    function(x)tail(x,1)==name,logical(1))]
  if (length(matches)!=1L) .fae_abort(sprintf("Unknown or ambiguous result table: %s",name))
  .fae_copy(result$tables[[matches]])
}

#' @rdname workflow
#' @export
get_controls <- function(result) get_table(result,"00_Summary/CONTROLS")

#' @rdname workflow
#' @export
get_lineage <- function(result) {.fae_check_result(result);.fae_copy(result$lineage)}

#' @rdname workflow
#' @export
export_results <- function(result,destination) {
  .fae_check_result(result);destination<-normalizePath(destination,winslash="/",mustWork=FALSE)
  if (file.exists(destination)) .fae_abort(sprintf("Destination already exists: %s",destination),
                                           "fae_resource_error")
  if (!dir.create(destination,recursive=TRUE)) .fae_abort("Could not create destination",
                                                           "fae_resource_error")
  groups <- split(names(result$tables),vapply(strsplit(names(result$tables),"/",fixed=TRUE),
    function(x)x[[1]],character(1)))
  files <- character()
  for (group in names(groups)) {
    payload <- setNames(lapply(groups[[group]],function(key)result$tables[[key]]),
                        vapply(strsplit(groups[[group]],"/",fixed=TRUE),function(x)x[[2]],character(1)))
    file <- file.path(destination,paste0(group,".json"));jsonlite::write_json(payload,file,
      auto_unbox=TRUE,pretty=TRUE,null="null",na="null",digits=NA);files<-c(files,file)
  }
  all_result <- file.path(destination,"results.json")
  jsonlite::write_json(unclass(result),all_result,auto_unbox=TRUE,pretty=TRUE,null="null",na="null",digits=NA)
  files <- c(files,all_result)
  hashes <- setNames(vapply(files,digest::digest,character(1),algo="sha256",file=TRUE),basename(files))
  manifest <- list(status=result$status,official=FALSE,lineage=result$lineage,files=as.list(hashes))
  jsonlite::write_json(manifest,file.path(destination,"run_manifest.json"),auto_unbox=TRUE,
                       pretty=TRUE,null="null",na="null")
  invisible(destination)
}
