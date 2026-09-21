root <- normalizePath(if (length(commandArgs(trailingOnly=TRUE)))
  commandArgs(trailingOnly=TRUE)[[1L]] else ".",winslash="/",mustWork=TRUE)
ns <- pkgload::load_all(root,quiet=TRUE)$env
exports <- sort(getNamespaceExports("financialAccountingEngine"))
groups <- list(
  `tests/testthat/test-formulas.R`=c("amortisation_schedule","amortised_cost_step",
    "benchmark_ecl","cash_shortfall_ecl","component_ecl","coverage_ratio",
    "cumulative_pd_from_conditional","discount_factor","ead_profile",
    "effective_interest_rate","hedge_ineffectiveness","lgd_from_recoveries",
    "liability_modification_test","marginal_pd_from_cumulative","modification_gain_loss",
    "poci_allowance_change","provision_matrix_ecl","recovery_present_value",
    "survival_from_pd","weighted_ecl"),
  `tests/testthat/test-resources.R`=c("export_profile","get_formula","get_parameters",
    "get_policy","get_schema","get_sources","list_profiles","load_reference_data",
    "select_snapshot","validate_dataset"),
  `tests/testthat/test-classification.R`=c("assess_business_model","assess_ibor_relief",
    "assess_reclassification","assess_recognition","assess_scope","assess_sicr",
    "assess_sppi","assess_stage","classify_instrument","impairment_scope",
    "initial_measurement","interest_revenue","present_fair_value","split_own_credit",
    "stage_movements"),
  `tests/testthat/test-risk-events.R`=c("allocate_credit_enhancements","allocate_overlay",
    "assess_derecognition","assess_hedge_effectiveness","continuing_involvement",
    "hedge_reserve_rollforward","measure_hedge","oci_rollforward","process_modification",
    "process_recovery","process_writeoff","reconcile_overlay","scenario_ecl",
    "validate_hedge_designation","validate_overlay"),
  `tests/testthat/test-reporting.R`=c("aggregate_disclosures","allowance_rollforward",
    "backtest_ead","backtest_lgd","backtest_models","backtest_pd","backtest_staging",
    "build_journals","compare_challenger","disclosure_facts","gross_rollforward",
    "reconcile_ledger","regulatory_bridge","scenario_sensitivity"),
  `tests/testthat/test-workflow.R`=c("export_results","get_controls","get_lineage",
    "get_metrics","get_table","run_dataset"))
test_file <- setNames(rep(names(groups),lengths(groups)),unlist(groups,use.names=FALSE))
rd_files <- list.files(file.path(root,"man"),pattern="\\.Rd$",full.names=TRUE)
rd <- setNames(lapply(rd_files,tools::parse_Rd),basename(rd_files))
aliases <- lapply(rd,function(page) {
  nodes <- Filter(function(x)identical(attr(x,"Rd_tag"),"\\alias"),page)
  trimws(vapply(nodes,function(x)paste(unlist(x),collapse=""),character(1)))
})
page_for <- function(export) {
  pages <- names(rd)[which(vapply(aliases,function(x)export %in% x,logical(1)))]
  if (length(pages)!=1L)
    stop("Expected exactly one Rd page for export '",export,"'; found ",length(pages))
  pages
}
rows <- lapply(exports,function(export) {
  lines <- readLines(file.path(root,test_file[[export]]),warn=FALSE)
  hit <- grep(paste0("test_that(\"",export),lines,fixed=TRUE,value=TRUE)
  test_id <- if (length(hit)) sub('.*test_that\\("([^"]+)".*','\\1',hit[[1L]]) else export
  data.frame(export=export,kind="function",
    signature=paste(names(formals(get(export,envir=ns))),collapse=","),rd_alias=export,
    rd_page=page_for(export),test_file=test_file[[export]],test_id=test_id,
    example_id=export,stringsAsFactors=FALSE)
})
inventory <- do.call(rbind,rows)
stopifnot(nrow(inventory)==80L,!anyNA(inventory),!anyDuplicated(inventory$export))
utils::write.csv(inventory,file.path(root,"tools","api_inventory.csv"),row.names=FALSE,
                 na="",quote=TRUE)
cat("API inventory written:",nrow(inventory),"exports\n")
