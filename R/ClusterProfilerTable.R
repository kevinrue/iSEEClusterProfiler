
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
  cmds <- "tab <- data.frame(GeneSet='GeneSet', p.value=0.05);"
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
