#' Make sequences of start and end months based on ymd input
#'
#' For a start and end date, formatted in 'ymd'-compatible format, create sequences
#'
#' @importFrom lubridate ymd days_in_month
#'
#' @param date_start A vector of length 1 denoting the start date range. Must be in ymd-compatible format
#' @param date_end A vector of length 1 denoting the end date range. Must be in ymd-compatible format
#'
#' @return
#' A named \code{list} of length 2, with as many elements as months based on the input start and end. One
#' list contains start dates, and the other end dates.
#'
#' All elements are returned as class \code{Date}.
#'
#' @export
#'
#' @examples
#' make_monthChunks("2017-01-01", "2017-03-01")
#' make_monthChunks(20170101, 20170301)
#' make_monthChunks("2017-Jan-1", 20170301)
#' make_monthChunks("2017-January 1", 20170301)
make_monthChunks <- function(date_start, date_end) {

  if(length(date_start) != 1L || length(date_end) != 1L) {
    stop("date_start and date_end must both be vectors of length 1")
  }

  start_parsed <- ymd(date_start)
  end_parsed   <- ymd(date_end)

  starts <- seq(as.Date(start_parsed), as.Date(end_parsed), by = "month")
  delta  <- days_in_month(starts) - 1
  ends   <- starts + delta

  out <- list(starts = starts, ends = ends)
  Map(unname, out)
}

