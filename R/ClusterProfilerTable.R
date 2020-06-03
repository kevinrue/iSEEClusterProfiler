#' clusterProfiler Result Table
#'
#' A table that dynamically computes pathway analysis results for a given gene set.
#'
#' @section Slot overview:
#' This class does not define any additional slots yet.
#'
#' This class inherits all slots from its parent
#' `Table-class` and `Panel-class` classes.
#'
#' @section Constructor:
#' `ClusterProfilerTable(...)` creates an instance of a ClusterProfilerTable class,
#' where any slot and its value can be passed to `...` as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, `x` is an instance of a [ClusterProfilerTable-class] class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For defining the interface:
#'
#' - `.fullName(x)` will return `"clusterProfiler Table"`.
#' - `.hideInterface(x)` will return \code{TRUE} for UI elements related to multiple column selections,
#' otherwise calling the method for \linkS4class{RowTable}.
#' - `.panelColor(x)` will return the specified default color for this panel class.
#'
#' For creating the table:
#'
#' - `.generateTable(x, envir)` will create a data.frame of newly computed statistics in \code{envir}.
#'   The method will return the commands required to do so.
#'
#' @name ClusterProfilerTable-class
#' @aliases ClusterProfilerTable ClusterProfilerTable-class
#' .fullName,ClusterProfilerTable-method
#' .panelColor,ClusterProfilerTable-method
#' .generateTable,ClusterProfilerTable-method
#' .hideInterface,ClusterProfilerTable-method
NULL

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
#' @importMethodsFrom iSEE .fullName
setMethod(".fullName", "ClusterProfilerTable", function(x) "clusterProfiler Table")

#' @export
#' @importMethodsFrom iSEE .panelColor
setMethod(".panelColor", "ClusterProfilerTable", function(x) "#111111")

#' @export
#' @importMethodsFrom iSEE .generateTable
#' @importFrom iSEE .textEval
setMethod(".generateTable", "ClusterProfilerTable", function (x, envir) {
  cmds <- .create_enrichgo_table(envir)

  .textEval(cmds, envir)
  cmds
}
)

#' @export
#' @importMethodsFrom iSEE .hideInterface
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
    return("tab <- data.frame(GeneSet=character(0), p.value=numeric(0))")
  }

  cmds <- ""

  cmds <- c(cmds, sprintf(
    ".results <- clusterProfiler::enrichGO(gene = row_selected$active, OrgDb = %s, keyType = %s)",
    deparse(iSEEOptions$get("orgdb.package")),
    deparse(iSEEOptions$get("orgdb.keytype"))))

  cmds <- c(cmds, "tab <- GeneTonic::shake_enrichResult(.results)")

  return(cmds)
}
