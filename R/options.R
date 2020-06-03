# Credits: Adapted from knitr
# https://github.com/yihui/knitr/blob/670a530b53d6cc5002797d54034ec8b07b74702c/R/defaults.R
#' @importFrom stats setNames
new_defaults = function(value = list()) {
  defaults = value

  get = function(name, default = FALSE, drop = TRUE) {
    if (default) defaults = value  # this is only a local version
    if (missing(name)) defaults else {
      if (drop && length(name) == 1) defaults[[name]] else {
        setNames(defaults[name], name)
      }
    }
  }
  resolve = function(...) {
    dots = list(...)
    if (length(dots) == 0) return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
      if (length(dots <- dots[[1]]) == 0) return()
    dots
  }
  set = function(...) {
    dots = resolve(...)
    if (length(dots)) defaults <<- merge(dots)
    invisible(NULL)
  }
  merge = function(values) merge_list(defaults, values)
  restore = function(target = value) defaults <<- target

  list(
    get = get, set = set,
    merge = merge, restore = restore
  )
}

#' Default panel options
#'
#' Set global options using \code{iSEEOptions$set()}, so that all relevant panels will use these default values during initialization.
#' This function mergees
#'
#' See \code{str(iSEEOptions$get())} for a list of default panel options.
#'
#' Note that \code{iSEEOptions$restore()} can be used to reset the global options to the package default values.
#'
#' @section Available options:
#'
#' \describe{
#' \item{\code{orgdb.package}}{Description.}
#' \item{\code{orgdb.key}}{Description.}
#' \item{\code{orgdb.keytype}}{Description.}
#' }
#'
#' @author Kevin Rue-Albrecht
#'
#' @export
#' @examples iSEEOptions$get('downsample'); iSEEOptions$get('org.db')
iSEEOptions <- new_defaults(c(iSEEOptions$get(), list(

  orgdb.package = NA_character_,
  orgdb.key = NA_character_,
  orgdb.keytype = NA_character_

)))

# merge elements of y into x with the same names
# Credits: knitr
merge_list = function(x, y) {
  x[names(y)] = y
  x
}
