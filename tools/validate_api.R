args <- commandArgs(trailingOnly=TRUE)
`%||%` <- function(x,y) if(is.null(x)) y else x
package <- if (length(args)>=1L) args[[1L]] else "financialAccountingEngine"
root <- normalizePath(if (length(args)>=2L) args[[2L]] else ".",winslash="/",mustWork=TRUE)
lib <- if (length(args)>=3L) normalizePath(args[[3L]],winslash="/",mustWork=TRUE) else NULL
ns <- if (is.null(lib)) pkgload::load_all(root,quiet=TRUE)$env else loadNamespace(package,lib.loc=lib)
inventory <- utils::read.csv(file.path(root,"tools","api_inventory.csv"),
                             stringsAsFactors=FALSE,check.names=FALSE)
required <- c("export","kind","signature","rd_alias","rd_page","test_file","test_id","example_id")
stopifnot(all(required %in% names(inventory)),nrow(inventory)==80L,
          !anyNA(inventory),!anyDuplicated(inventory$export),
          all(inventory$kind=="function"),
          setequal(getNamespaceExports(package),inventory$export))
if (is.null(lib)) {
  rd_files <- list.files(file.path(root,"man"),pattern="\\.Rd$",full.names=TRUE)
  rd <- setNames(lapply(rd_files,tools::parse_Rd),basename(rd_files))
} else {
  rd <- tools::Rd_db(package,lib.loc=lib)
}
tag_values <- function(page,tag) {
  nodes <- Filter(function(x)identical(attr(x,"Rd_tag"),tag),page)
  trimws(vapply(nodes,function(x)paste(unlist(x),collapse=""),character(1)))
}
aliases <- lapply(rd,tag_values,tag="\\alias")
for (i in seq_len(nrow(inventory))) {
  item <- inventory[i,]
  pages <- which(vapply(aliases,function(x)item$rd_alias %in% x,logical(1)))
  stopifnot(length(pages)==1L)
  page <- rd[[pages]]
  tags <- vapply(page,function(x)attr(x,"Rd_tag") %||% "",character(1))
  needed <- c("\\title","\\description","\\usage","\\arguments","\\value",
              "\\details","\\references","\\seealso","\\examples")
  stopifnot(all(needed %in% tags),all(vapply(needed,function(tag)
    any(nzchar(tag_values(page,tag))),logical(1))))
  examples <- paste(tag_values(page,"\\examples"),collapse="\n")
  stopifnot(grepl(paste0("(^|[^[:alnum:]_.])",item$example_id,
                        "[[:space:]]*\\("),examples,perl=TRUE))
  test <- readLines(file.path(root,item$test_file),warn=FALSE)
  stopifnot(any(grepl(item$test_id,test,fixed=TRUE)))
  fn <- get(item$export,envir=ns,inherits=FALSE)
  stopifnot(is.function(fn),identical(paste(names(formals(fn)),collapse=","),item$signature))
}
cat("API/Rd/test inventory: PASS (80 exports)\n")
