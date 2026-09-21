args <- commandArgs(trailingOnly=TRUE)
package <- if(length(args)>=1L) args[[1L]] else "financialAccountingEngine"
lib <- if(length(args)>=2L) normalizePath(args[[2L]],winslash="/",mustWork=TRUE) else .libPaths()
ns <- loadNamespace(package,lib.loc=lib)
call <- function(name,...) do.call(get(name,envir=ns),list(...))
result <- call("run_dataset",call("load_reference_data","SMALL_SA_RETAIL_FIA"))
stopifnot(result$status=="APPROVED_REFERENCE",length(call("get_controls",result))==28L,
          all(vapply(call("get_controls",result),function(x)x$status=="PASS",logical(1))))
rejected <- inherits(try(call("validate_dataset",list()),silent=TRUE),"try-error")
stopifnot(rejected)
cat("Installed domain run, controls and rejection smoke test: PASS\n")
