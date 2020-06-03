
#' @export
setClass("ClusterProfilerTable", contains="Table")

#' @importFrom S4Vectors setValidity2
setValidity2("ClusterProfilerTable", function(object) {
  msg <- character(0)

  if (length(msg)) {
    return(msg)
  }
  TRUE
})

#' @export
#' @importFrom methods new
ClusterProfilerTable <- function(...) {
  new("ClusterProfilerTable", ...)
}

#' @export
setMethod(".fullName", "ClusterProfilerTable", function(x) "clusterProfiler Table")

#' @export
setMethod(".panelColor", "ClusterProfilerTable", function(x) "#111111")

#' @export
setMethod(".generateTable", "ClusterProfilerTable", function (x, envir) {
  cmds <- .create_enrichgo_table(envir)

  .textEval(cmds, envir)
  cmds
}
)

#' @export
setMethod(".hideInterface", "ClusterProfilerTable", function(x, field) {
  if (field %in% c(
    "ColumnSelectionSource",
    "ColumnSelectionType",
    "ColumnSelectionSaved",
    "ColumnSelectionDynamicSource")) {
    TRUE
  } else if (field %in% "ColumnSelectionSource") {
    FALSE
  } else {
    callNextMethod()
  }
})

#' @importFrom clusterProfiler enrichGO
#' @importFrom GeneTonic shake_enrichResult
.create_enrichgo_table <- function(envir) {
  # TODO: disable panel if iSEEOptions$get("orgdb.package") is not installed
  # TODO: set a better default when there is no incoming selection

  if (!exists("row_selected", envir=envir, inherits=FALSE)) {
    return("tab <- data.frame(GeneSet=character(0), p.value=numeric(0));")
  }

  cmds <- ""

  cmds <- c(cmds, sprintf(
    ".results <- clusterProfiler::enrichGO(gene = geneids, OrgDb = %s, keyType = %s)",
    deparse(iSEEOptions$get("orgdb.package")),
    deparse(iSEEOptions$get("orgdb.keytype"))))

  cmds <- c(cmds, "tab <- GeneTonic::shake_enrichResult(.results)")

  return(cmds)
}
