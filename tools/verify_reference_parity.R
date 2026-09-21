args <- commandArgs(trailingOnly=TRUE)
`%||%` <- function(x,y) if (is.null(x)) y else x
if (length(args)>=2L) {
  package <- args[[1L]]
  lib <- normalizePath(args[[2L]],winslash="/",mustWork=TRUE)
  ns <- loadNamespace(package,lib.loc=lib)
  report_dir <- if (length(args)>=3L) args[[3L]] else tempfile("fae-parity-")
} else {
  lib <- NULL
  package <- "financialAccountingEngine"
  root <- normalizePath(".",winslash="/",mustWork=TRUE)
  ns <- pkgload::load_all(root,quiet=TRUE)$env
  report_dir <- file.path(root,".work","parity")
}
dir.create(report_dir,recursive=TRUE,showWarnings=FALSE)
call <- function(name,...) do.call(get(name,envir=ns,inherits=FALSE),list(...))
resource <- system.file("extdata","golden.json",package=package,lib.loc=lib %||% NULL)
if (!nzchar(resource)) resource <- file.path("inst","extdata","golden.json")
golden <- jsonlite::fromJSON(resource,simplifyVector=FALSE)
decode <- function(x) {
  if (is.list(x) && identical(names(x),"decimal")) return(as.numeric(x$decimal))
  if (is.list(x)) {
    result <- x
    for (i in seq_along(x)) result[i] <- list(decode(x[[i]]))
    return(result)
  }
  x
}
golden <- decode(golden)
ignored <- unlist(golden$ignored_technical_fields,use.names=FALSE)
normalize <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) x <- x[setdiff(names(x),ignored)]
    result <- x
    for (i in seq_along(x)) result[i] <- list(normalize(x[[i]]))
    if (!is.null(names(result)) && "explanation" %in% names(result) &&
        (is.null(result$explanation) || identical(result$explanation,""))) {
      result["explanation"] <- list("")
    }
    return(result)
  }
  x
}
compare <- function(actual,expected,path="root") {
  if (is.null(expected) || (length(expected)==1L && is.atomic(expected) && is.na(expected))) {
    if (is.null(actual) || (length(actual)==1L && is.atomic(actual) && is.na(actual))) return(invisible())
    stop(path,": expected null/missing value")
  }
  if (is.null(actual) || (length(actual)==1L && is.atomic(actual) && is.na(actual)))
    stop(path,": actual value is null/missing")
  if (is.numeric(expected)) {
    if (!is.numeric(actual) || length(actual)!=length(expected) ||
        any(!is.finite(actual)) || any(abs(actual-expected) >
          as.numeric(golden$absolute_tolerance)+as.numeric(golden$relative_tolerance)*abs(expected)))
      stop(path,": numeric difference: actual=",paste(actual,collapse=","),
           " expected=",paste(expected,collapse=","))
    return(invisible())
  }
  if (is.list(expected)) {
    if (!is.list(actual) || length(actual)!=length(expected))
      stop(path,": list length/type difference: ",length(actual)," vs ",length(expected))
    if (!setequal(names(actual),names(expected)) || length(names(actual))!=length(names(expected)))
      stop(path,": names differ: actual=",paste(names(actual),collapse=","),
           " expected=",paste(names(expected),collapse=","))
    for (i in seq_along(expected)) {
      label <- if (!is.null(names(expected)) && nzchar(names(expected)[[i]])) names(expected)[[i]] else i
      actual_value <- if (!is.null(names(expected))) actual[[label]] else actual[[i]]
      compare(actual_value,expected[[i]],paste0(path,".",label))
    }
    return(invisible())
  }
  if (!identical(actual,expected)) stop(path,": actual=",actual," expected=",expected)
  invisible()
}
reports <- list()
for (profile_id in call("list_profiles")) {
  result <- call("run_dataset",call("load_reference_data",profile_id))
  expected <- golden$profiles[[profile_id]]
  compare(result$status,expected$status,paste(profile_id,"status",sep="."))
  compare(normalize(result$metrics),normalize(expected$metrics),paste(profile_id,"metrics",sep="."))
  compare(normalize(result$tables),normalize(expected$tables),paste(profile_id,"tables",sep="."))
  reports[[profile_id]] <- list(status="PASS",table_count=length(result$tables),
    control_count=length(call("get_controls",result)))
}
jsonlite::write_json(list(status="PASS",profiles=reports),file.path(report_dir,"parity-report.json"),
                     auto_unbox=TRUE,pretty=TRUE)
cat("Complete Python/R/golden reference parity: PASS\n")
