#' Append value(s) to a named list element
#'
#' Append a vector of values to a specific named element in a list.
#'
#' @param x Values to append
#' @param lst The list to append to. Must be named and contain the name as denoted by \emph{targ}.
#' @param targ The element in \emph{lst} to append to
#'
#' @return
#' The input list, with values within \emph{val} appended to \emph{targ}.
#'
#' @details
#' This is simply a convenient way to append a vector of values to a named list element. It is
#' convenient in the sense that instead of:
#'
#' \code{lst[["targ"]] <- c(lst[["targ"]], x)},
#'
#' you can do:
#'
#' \code{lst <- append_el(x, lst = lst, targ = "targ")}
#'
#' The main use case is e.g. appending one or more segment(ids) to a base argument list for
#' comparison and/or testing.
#'
#' @export
#'
#' @examples
#' # TBD
append_el <- function(x, lst, targ) {
  if(!is.list(lst)) {
    stop("lst must be a list")
  }
  if(length(targ) > 1L || !is.character(targ)) {
    stop("targ must be a character vector of length 1")
  }
  if(! targ %in% names(lst)) {
    stop("targ not found within names(lst)")
  }

  # warn on duplicates in x
  if(any(duplicated(x))) {
    warning("duplicated values found within 'x'")
  }
  # warn on duplicates in resulting output
  if(any(duplicated(c(lst[[targ]], x)))) {
    warning(
      paste(
        "appending 'x' to",
        substitute(targ),
        "element of",
        substitute(lst),
        "resulted in duplicate values"
      )
    )

  }

  lst[[targ]] <- c(lst[[targ]], x)
  lst
}
