#' financial-accounting-engine: Native Financial-Instrument Accounting
#'
#' `financialAccountingEngine` provides 80 granular, native R functions for
#' financial-instrument classification, measurement, effective interest,
#' impairment, hedge accounting, journals, disclosures, model monitoring,
#' reconciliation, controls, and audit lineage. It also provides a controlled
#' end-to-end reference workflow over two fully synthetic offline profiles.
#'
#' @details Formula functions accept ordinary R vectors, matrices, and lists.
#'   Domain functions accept named canonical rows and tables. The workflow uses
#'   the same public functions, returns an `fae_analysis_result`, and exposes
#'   30 named result tables, 28 controls, parameters, metrics, and lineage through
#'   public accessors. Rates and probabilities use decimal notation; time is in
#'   years; monetary inputs in one calculation use one currency unless an
#'   explicit FX mapping is supplied. Missing judgements, approvals, and model
#'   inputs are rejected rather than silently inferred.
#'
#' @section Where to start:
#' - Use [discount_factor()], [effective_interest_rate()], and [component_ecl()]
#'   for independent formula calculations.
#' - Use [assess_sppi()], [classify_instrument()], and [assess_stage()] for
#'   row-level accounting decisions.
#' - Use [load_reference_data()] and [run_dataset()] for a complete synthetic
#'   reference run, then inspect it with [get_metrics()], [get_table()],
#'   [get_controls()], and [get_lineage()].
#' - Read `vignette("getting-started")` first; the other vignettes cover
#'   impairment, hedges, journals, disclosures, reference data, and controls.
#'
#' @section Reference data and status:
#' `SMALL_SA_RETAIL_FIA` and `MID_SIZE_UNIVERSAL_FIA` are deterministic,
#' synthetic regression fixtures. `APPROVED_REFERENCE` means that the package's
#' technical controls passed. It is not an institutional, audit, or production
#' approval.
#'
#' @section Legal and professional scope:
#' This independent package is not affiliated with or endorsed by the IFRS
#' Foundation or the International Accounting Standards Board. It is reference
#' software under Apache License 2.0. It does not provide accounting, audit,
#' legal, investment, prudential, or tax advice and does not replace
#' authoritative standards, entity policies, professional judgement, model
#' validation, or financial-close controls.
#' @references <https://www.ifrs.org/issued-standards/list-of-standards/ifrs-9-financial-instruments/>
#' @seealso [core_formulas], [classification_decisions], [risk_event_hedge],
#'   [reporting_monitoring], [reference_data], [workflow]
#' @examples
#' discount_factor(0.05, 1)
#' result <- run_dataset(load_reference_data("SMALL_SA_RETAIL_FIA"))
#' stopifnot(
#'   result$status == "APPROVED_REFERENCE",
#'   length(get_table(result, "DISCLOSURE_FACTS")) > 0L,
#'   all(vapply(get_controls(result), function(x) x$status == "PASS", logical(1)))
#' )
#' @importFrom stats setNames
#' @importFrom utils tail
#' @keywords package
"_PACKAGE"
