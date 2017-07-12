#' Clean up RSiteCatalyst Queue* query returns
#'
#' For a query return, transform to data.table, handle POSIXlt, and drop columns that are all blank
#'
#' @importFrom wzMisc cols_class remove_ifall_cols
#' @import data.table
#'
#' @param x A call return from an \code{RSiteCatalyst} Queue* function. Expected to be a
#' \code{data.frame}.
#'
#' @return
#' A \code{data.table}, with any \code{POSIXlt} or \code{POSIXct} columns coerced to the \code{data.table}
#' \code{IDate} class. Additionally, any columns where all values are blank are dropped, with a message.
#'
#' @export
#'
#' @examples
#' #TBD
process_query_ret <- function(x) {
  stopifnot(is.data.frame(x))

  if(!is.data.table(x)) {
    x <- as.data.table(x)
  }

  # handle POSIXlt
  lt_cols <- cols_class("POSIXlt", "POSIXct")
  tryCatch(
    {
      targ <- lt_cols(x)
      for(i in seq_along(targ)) {
        x[[targ[[i]]]] <- as.IDate(x[[targ[[i]]]])
      }
    },
    error = function(e) {
      invisible(e)
      x <- x
    }
  )

  remove_ifall_cols(df = x, ifallwhat = "")

}
