#' Make sequences of time intervals
#'
#' For a start and end date, formatted in 'ymd'-compatible format, create sequences
#'
#' @importFrom lubridate ymd days weeks years
#'
#' @param date_start (Required) A vector of length 1 denoting the start date range.
#' Must be in ymd-compatible format
#' @param date_end (Optional) A vector of length 1 denoting the end date range.
#' If provided, must be in ymd-compatible format. Defaults to current day or overriden by \emph{n}.
#' @param period_type (Required) One of \code{days, weeks, months, years} to denote the interval
#' @param n (Optional) An integer vector of length 1, dicatating the length of the output.
#' Overrides \emph{date_end} if both supplied.
#'
#' @return
#' A list of length 2, with named \emph{Date} elements of \code{start, end}. Each respective entity \code{[[i]]}
#' within each element \code{[i]} describes a pair of start and end periods.
#'
#' @details
#' This is a more flexible version of \link{make_monthChunks}, providing interval granularity of
#' \emph{days, weeks, months, years}.
#'
#' @export
#'
#' @examples
#' pers <- setNames(nm = c("days", "weeks", "months", "years"))
#' # Variable length, depending on interval type
#' lapply(pers, function(f) make_timeChunks(date_start = "2017-01-01", period_type = f))
#' # Fixed length, using n
#' lapply(pers, function(f) make_timeChunks(date_start = "2017-01-01", period_type = f, n = 5))
#' # Output as data.frame
#' as.data.frame(
#'   do.call(cbind,
#'           Map(as.character,
#'               make_timeChunks(date_start = "2017-01-01",
#'                               period_type = "weeks",
#'                               n = 4)
#'           )
#'   ),
#'   stringsAsFactors = FALSE
#' )
make_timeChunks <- function(date_start, date_end = NULL, period_type = NULL, n = NULL) {

  funlist <- list(
    days = lubridate::days,
    weeks = lubridate::weeks,
    months = base::months,
    years = lubridate::years
  )

  if(is.null(period_type)) {
    stop("'period_type' must be provided'")
  }
  if(length(period_type) > 1L || !is.character(period_type)) {
    stop("'period_type' must be a character vector of length 1")
  }
  if(!period_type %in% names(funlist)) {
    stop("period_type must be one of {",
         paste(names(funlist), collapse = ", "), "}")
  }

  period_fun <- funlist[[period_type]]

  if(!is.null(n) && !is.null(date_end)) {
    warning("Both 'n' and 'date_end' supplied; ignoring 'date_end' and using 'n' only")
    date_end <- ymd(date_start) + period_fun(n)
  }
  if(is.null(date_end) && is.null(n)) {
    date_end <- ymd(Sys.Date())
  }
  # handle n
  if(!is.null(n)) {
    if(length(n) > 1L) {
      stop("'n' must be an integer vector of length 1")
    }
    nper <- as.integer(n)
  } else {
    nper <- length(seq.Date(from = ymd(date_start),
                            to = ymd(date_end),
                            by = period_type)
    )
  }

  starts <- ymd(date_start) + period_fun(seq_len(nper) - 1)
  ends   <- starts + period_fun(1) - "days"(1)

  out <- list(
    start = starts,
    end   = ends
  )

  out
}
