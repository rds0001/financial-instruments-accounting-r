.fae_read_json <- function(filename) {
  path <- system.file("extdata", filename, package = "financialAccountingEngine")
  if (!nzchar(path) || !file.exists(path)) {
    .fae_abort(sprintf("Unknown packaged resource: %s", filename), "fae_resource_error")
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

.fae_policy <- function() {
  list(
    policy_id = "EU_FINANCIAL_INSTRUMENTS_2026_V1", version = "1.0.0",
    reporting_date = "2026-12-31", knowledge_time = "2026-09-02T12:00:00+02:00",
    jurisdiction = "EU_DE", reporting_scope = "CONSOLIDATED_GROUP",
    presentation_currency = "EUR", regular_way_policy = "TRADE_DATE",
    electronic_payment_derecognition_election = TRUE,
    hedge_accounting_policy = "IFRS9_GENERAL", low_credit_risk_relief = FALSE,
    past_due_sicr_days = 30, default_backstop_days = 90,
    rating_notch_sicr = 2, absolute_lifetime_pd_sicr = 0.05,
    relative_lifetime_pd_sicr = 2, cure_months = 3, probation_months = 12,
    scenario_weight_tolerance = 1e-9, money_rounding = "HALF_EVEN",
    money_decimals = 2, day_count = "ACT_365",
    official_status_limit = "APPROVED_REFERENCE",
    source_rule_set = "FIA_RULES_2026_V1"
  )
}

.fae_formulas <- function() {
  ids <- c("F-INIT-001", "F-EIR-001", "F-AC-001", "F-PD-001", "F-ECL-001",
           "F-CS-001", "F-PM-001", "F-MOD-001", "F-HDG-001", "F-OWNCR-001",
           "F-DEREC-001", "F-WO-001", "F-REC-001", "F-COL-001", "F-BT-001",
           "F-AGG-001", "F-ROLL-001", "F-REG-BRIDGE-001", "F-FV-001",
           "F-SENS-001", "F-OCI-ROLL-001")
  names <- c("initial_measurement", "effective_interest_rate", "amortised_cost_step",
             "marginal_pd_from_cumulative", "component_ecl", "cash_shortfall_ecl",
             "provision_matrix_ecl", "modification_gain_loss", "hedge_ineffectiveness",
             "own_credit_split", "derecognition_decision", "writeoff", "recovery",
             "credit_enhancement_allocation", "model_backtesting_metrics",
             "disclosure_aggregation", "balance_rollforward", "regulatory_bridge",
             "fair_value_presentation", "scenario_sensitivity",
             "oci_reserve_rollforward")
  Map(function(id, name) list(id = id, version = "1.0.0", name = name), ids, names)
}

.fae_timestamp <- function(value) {
  normalized <- sub("Z$", "+0000", value)
  normalized <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", normalized)
  result <- as.POSIXct(normalized, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  if (is.na(result)) .fae_abort(sprintf("Invalid timestamp: %s", value))
  result
}

.fae_validation_abort <- function(code, object, message, severity = "ERROR") {
  issues <- list(list(severity = severity, code = code, object = object, message = message))
  .fae_abort(message, "fae_validation_error", issues)
}

#' Packaged Policies, Schemas and Synthetic Reference Profiles
#'
#' Access versioned policy parameters, formula metadata, source metadata,
#' canonical table schemas and two complete synthetic input profiles. Validate
#' a dataset or select the unique official bitemporal record at explicit
#' reporting and knowledge cut-offs. All returned objects are independent R
#' lists and no function performs a network request.
#'
#' @param policy_id Explicit accounting-policy identifier. Only the packaged
#'   `EU_FINANCIAL_INSTRUMENTS_2026_V1` policy is accepted.
#' @param table Optional canonical table name. `NULL` returns all 19 schemas.
#' @param formula_id Formula registry identifier such as `"F-ECL-001"`.
#' @param rule_set_id Explicit rule-set identifier. Only
#'   `FIA_RULES_2026_V1` is packaged.
#' @param profile_id One value returned by `list_profiles()`.
#' @param destination A new directory path. Existing paths are rejected.
#' @param tables Named list containing canonical row lists for all 19 tables,
#'   or a subset for `select_snapshot()`.
#' @param reporting_date ISO date (`YYYY-MM-DD`) used for the valid-time cut-off.
#' @param knowledge_time ISO timestamp with explicit UTC offset used for the
#'   transaction-time cut-off.
#'
#' @return `get_policy()` and `get_parameters()` return a named parameter list;
#'   `get_schema()` returns one schema or all schemas; `get_formula()` returns
#'   one registry row; `get_sources()` returns a list of source-metadata rows;
#'   `list_profiles()` returns two stable character identifiers;
#'   `load_reference_data()`, `select_snapshot()` and `validate_dataset()`
#'   return named lists of row lists; `export_profile()` invisibly returns the
#'   normalized destination path after writing 19 JSON table files and a JSON
#'   manifest.
#'
#' @details Resources are synthetic and bundled for deterministic offline use.
#'   `select_snapshot()` requires exactly one `ACTIVE`, official version per
#'   business key whose valid and known intervals include both cut-offs.
#'   `validate_dataset()` checks the exact table/column contract, primary keys,
#'   typed numeric/logical fields, supported view, scenario weights, curve keys
#'   and positive FX rates before returning the selected snapshot. It raises a
#'   condition inheriting from `fae_validation_error` on rejection.
#'
#' @references International Accounting Standards Board (2014), IFRS 9,
#'   paragraphs 5.5.3--5.5.17. Official source index:
#'   <https://www.ifrs.org/issued-standards/list-of-standards/ifrs-9-financial-instruments/>.
#'
#' @seealso [run_dataset()], [core_formulas]
#'
#' @examples
#' list_profiles()
#' small <- load_reference_data("SMALL_SA_RETAIL_FIA")
#' length(small$instruments)
#' get_schema("instruments")$columns[1:3]
#' get_policy()$past_due_sicr_days
#' get_parameters()$default_backstop_days
#' get_formula("F-ECL-001")$name
#' length(get_sources())
#' selected <- select_snapshot(
#'   list(instruments = small$instruments), "2026-12-31",
#'   "2026-09-02T12:00:00+02:00"
#' )
#' stopifnot(length(selected$instruments) == 5L)
#' validated <- validate_dataset(small)
#' stopifnot(length(validated) == 19L)
#' target <- tempfile("fae-profile-")
#' export_profile("SMALL_SA_RETAIL_FIA", target)
#' unlink(target, recursive = TRUE)
#' @name reference_data
NULL

#' @rdname reference_data
#' @export
get_policy <- function(policy_id = "EU_FINANCIAL_INSTRUMENTS_2026_V1") {
  if (!identical(policy_id, "EU_FINANCIAL_INSTRUMENTS_2026_V1")) {
    .fae_abort(sprintf("Unknown policy_id: %s", policy_id), "fae_resource_error")
  }
  .fae_policy()
}

#' @rdname reference_data
#' @export
get_parameters <- function(policy_id = "EU_FINANCIAL_INSTRUMENTS_2026_V1") {
  get_policy(policy_id)
}

#' @rdname reference_data
#' @export
get_schema <- function(table = NULL) {
  schema <- .fae_read_json("schema.json")
  if (is.null(table)) return(schema)
  if (!is.character(table) || length(table) != 1L || is.na(table) ||
      is.null(schema[[table]])) {
    .fae_abort(sprintf("Unknown input table: %s", paste(table, collapse = "")),
               "fae_resource_error")
  }
  schema[[table]]
}

#' @rdname reference_data
#' @export
get_formula <- function(formula_id, rule_set_id = "FIA_RULES_2026_V1") {
  if (!identical(rule_set_id, "FIA_RULES_2026_V1")) {
    .fae_abort(sprintf("Unknown rule_set_id: %s", rule_set_id), "fae_resource_error")
  }
  hits <- Filter(function(x) identical(x$id, formula_id), .fae_formulas())
  if (length(hits) != 1L) {
    .fae_abort(sprintf("Unknown formula: %s", formula_id), "fae_resource_error")
  }
  hits[[1L]]
}

#' @rdname reference_data
#' @export
get_sources <- function() .fae_read_json("sources.json")$sources

#' @rdname reference_data
#' @export
list_profiles <- function() c("MID_SIZE_UNIVERSAL_FIA", "SMALL_SA_RETAIL_FIA")

#' @rdname reference_data
#' @export
load_reference_data <- function(profile_id) {
  if (!is.character(profile_id) || length(profile_id) != 1L ||
      !profile_id %in% list_profiles()) {
    .fae_abort(sprintf("Unknown profile: %s", paste(profile_id, collapse = "")),
               "fae_resource_error")
  }
  .fae_read_json(paste0(profile_id, ".json"))
}

#' @rdname reference_data
#' @export
export_profile <- function(profile_id, destination) {
  data <- load_reference_data(profile_id)
  destination <- normalizePath(destination, winslash = "/", mustWork = FALSE)
  if (file.exists(destination)) {
    .fae_abort(sprintf("Export destination already exists: %s", destination),
               "fae_resource_error")
  }
  if (!dir.create(destination, recursive = TRUE, showWarnings = FALSE)) {
    .fae_abort(sprintf("Could not create export destination: %s", destination),
               "fae_resource_error")
  }
  ok <- vapply(names(data), function(name) {
    path <- file.path(destination, paste0(name, ".json"))
    jsonlite::write_json(data[[name]], path, auto_unbox = TRUE, pretty = TRUE,
                         null = "null", na = "null", digits = NA)
    file.exists(path)
  }, logical(1))
  manifest <- list(profile_id = profile_id, profile_version = "1.0.0",
                   contract_version = "1.1.0", format = "canonical-json",
                   files = paste0(names(data), ".json"),
                   row_counts = vapply(data, length, integer(1)))
  jsonlite::write_json(manifest, file.path(destination, "profile_manifest.json"),
                       auto_unbox = TRUE, pretty = TRUE)
  if (!all(ok)) .fae_abort("One or more profile tables could not be exported",
                            "fae_resource_error")
  invisible(destination)
}

#' @rdname reference_data
#' @export
select_snapshot <- function(tables, reporting_date, knowledge_time) {
  if (!is.list(tables) || is.null(names(tables)) || any(!nzchar(names(tables)))) {
    .fae_abort("tables must be a named list")
  }
  cut_date <- as.Date(reporting_date)
  if (is.na(cut_date)) .fae_abort("reporting_date must be an ISO date")
  cut_time <- .fae_timestamp(knowledge_time)
  selected <- vector("list", length(tables))
  names(selected) <- names(tables)
  issues <- list()
  for (table_name in names(tables)) {
    rows <- tables[[table_name]]
    if (!is.list(rows)) .fae_validation_abort("ROW_SET", table_name, "Table must contain rows")
    keys <- unique(vapply(rows, function(x) as.character(x$business_key %||% ""), character(1)))
    selected[[table_name]] <- list()
    for (key in keys) {
      applicable <- Filter(function(row) {
        identical(as.character(row$business_key %||% ""), key) &&
          isTRUE(row$is_official) && identical(row$record_status, "ACTIVE") &&
          !is.na(as.Date(row$valid_from)) && !is.na(as.Date(row$valid_to)) &&
          as.Date(row$valid_from) <= cut_date && cut_date <= as.Date(row$valid_to) &&
          .fae_timestamp(row$known_from) <= cut_time && cut_time <= .fae_timestamp(row$known_to)
      }, rows)
      if (length(applicable) != 1L) {
        issues[[length(issues) + 1L]] <- list(
          severity = "ERROR", code = "OFFICIAL_SNAPSHOT_CARDINALITY",
          object = paste(table_name, key, sep = ":"),
          message = sprintf("Found %d instead of one official version", length(applicable)))
      } else selected[[table_name]][[length(selected[[table_name]]) + 1L]] <- applicable[[1L]]
    }
  }
  if (length(issues)) .fae_abort("Bitemporal snapshot rejected", "fae_validation_error", issues)
  selected
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' @rdname reference_data
#' @export
validate_dataset <- function(tables, reporting_date = NULL, knowledge_time = NULL) {
  schemas <- get_schema()
  if (!is.list(tables) || !setequal(names(tables), names(schemas)) ||
      length(tables) != length(schemas)) {
    .fae_validation_abort("TABLE_SET", "dataset",
                          "Exactly the 19 canonical tables are required", "FATAL")
  }
  for (table_name in names(schemas)) {
    rows <- tables[[table_name]]
    expected <- unlist(schemas[[table_name]]$columns, use.names = FALSE)
    fields <- schemas[[table_name]]$fields
    ids <- character()
    for (row in rows) {
      if (!is.list(row) || !setequal(names(row), expected) || length(row) != length(expected)) {
        .fae_validation_abort("ROW_SCHEMA", table_name,
                              "Columns differ from the canonical schema")
      }
      required <- c("record_id", "business_key", "version_no", "valid_from", "valid_to",
                    "known_from", "known_to", "record_status")
      if (any(vapply(required, function(k) is.null(row[[k]]) ||
                     !length(row[[k]]) || identical(row[[k]], ""), logical(1)))) {
        .fae_validation_abort("ROW_SCHEMA", table_name, "Required metadata are missing")
      }
      id <- as.character(row$record_id)
      if (id %in% ids) .fae_validation_abort("PRIMARY_KEY", table_name, "Duplicate record_id")
      ids <- c(ids, id)
      for (field in fields) {
        value <- row[[field$field]]
        if (!is.null(value) && identical(field$type, "number")) .fae_number(value, field$field)
        if (!is.null(value) && identical(field$type, "boolean") &&
            (!is.logical(value) || length(value) != 1L || is.na(value))) {
          .fae_validation_abort("ROW_SCHEMA", table_name,
                                sprintf("%s: logical value required", field$field))
        }
      }
    }
  }
  rc <- tables$run_control
  if (length(rc) != 1L) .fae_validation_abort("RUN_CONTROL", "run_control",
                                               "Exactly one run-control row is required")
  if (!rc[[1L]]$view %in% c("REFERENCE", "MANAGEMENT")) {
    .fae_validation_abort("OFFICIAL_BLOCKED", "run_control",
                          "Only REFERENCE or MANAGEMENT view is supported")
  }
  rd <- reporting_date %||% rc[[1L]]$reporting_date
  kt <- knowledge_time %||% rc[[1L]]$knowledge_time
  selected <- select_snapshot(tables, rd, kt)
  probabilities <- vapply(selected$scenarios, function(x) .fae_number(x$probability,
                                                                      "probability", 0, 1), numeric(1))
  if (!length(probabilities) || abs(sum(probabilities) - 1) > 1e-9) {
    .fae_validation_abort("SCENARIO_WEIGHTS", "scenarios",
                          "Scenario probabilities must sum to one")
  }
  for (table_name in c("pd_curves", "lgd_profiles", "ead_profiles")) {
    keys <- vapply(selected[[table_name]], function(x) {
      paste(x$instrument_id, x$scenario_id, x$year, sep = "|")
    }, character(1))
    if (anyDuplicated(keys)) .fae_validation_abort("CURVE_KEY", table_name,
                                                    "Duplicate curve key")
  }
  for (row in selected$market_data) {
    if (.fae_number(row$fx_rate, "fx_rate", 0) == 0) {
      .fae_validation_abort("FX_RATE", "market_data", "Positive FX rate required")
    }
  }
  selected
}
