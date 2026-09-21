# Internal validation and typed condition helpers.

.fae_abort <- function(message, subclass = "fae_accounting_error", issues = NULL) {
  condition <- structure(
    list(message = as.character(message), call = NULL, issues = issues),
    class = c(subclass, "fae_accounting_error", "error", "condition")
  )
  stop(condition)
}

.fae_number <- function(value, name, minimum = NULL, maximum = NULL) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value)) {
    .fae_abort(sprintf("%s: one finite numeric value is required", name))
  }
  value <- as.numeric(value)
  if ((!is.null(minimum) && value < minimum) ||
      (!is.null(maximum) && value > maximum)) {
    .fae_abort(sprintf("%s: value outside the permitted range", name))
  }
  value
}

.fae_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    .fae_abort(sprintf("%s: one non-missing logical value is required", name))
  }
  value
}

.fae_vector <- function(value, name, minimum = NULL, maximum = NULL,
                        allow_empty = TRUE) {
  if (!is.numeric(value) || anyNA(value) || any(!is.finite(value)) ||
      (!allow_empty && length(value) == 0L)) {
    .fae_abort(sprintf("%s: a finite numeric vector is required", name))
  }
  value <- as.numeric(value)
  if ((!is.null(minimum) && any(value < minimum)) ||
      (!is.null(maximum) && any(value > maximum))) {
    .fae_abort(sprintf("%s: values outside the permitted range", name))
  }
  value
}

.fae_cashflows <- function(value, name, allow_empty = TRUE) {
  if (is.data.frame(value)) value <- as.matrix(value)
  if (is.list(value) && !is.matrix(value)) {
    if (!length(value)) value <- matrix(numeric(), ncol = 2L)
    else value <- do.call(rbind, value)
  }
  if (!is.matrix(value) || ncol(value) != 2L ||
      (!allow_empty && nrow(value) == 0L)) {
    .fae_abort(sprintf("%s: supply a two-column matrix/data frame or list of pairs", name))
  }
  storage.mode(value) <- "double"
  if (anyNA(value) || any(!is.finite(value)) || any(value < 0)) {
    .fae_abort(sprintf("%s: times and amounts must be finite and non-negative", name))
  }
  colnames(value) <- c("time", "amount")
  value
}

.fae_weights <- function(values) {
  if (is.null(names(values)) || any(!nzchar(names(values))) || anyDuplicated(names(values))) {
    .fae_abort("Scenario probabilities require unique, non-empty names")
  }
  result <- .fae_vector(values, "scenario probabilities", 0, 1, allow_empty = FALSE)
  names(result) <- names(values)
  if (abs(sum(result) - 1) > 1e-9) {
    .fae_abort("Scenario probabilities must sum to one (absolute tolerance 1e-9)")
  }
  result
}
