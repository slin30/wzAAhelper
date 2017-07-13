#' Check current queue with messaging
#'
#' Automatically check the current queue at pre-determined time intervals
#'
#' @importFrom RSiteCatalyst GetQueue
#'
#' @param x (optional) integer vector of report IDs to check. If not provided, will use the starting
#' set found via \code{GetQueue}.
#' @param base (optional) numeric vector of length 1. The base to use to (exponentially) increment
#' wait times between iterations. Defaults to \code{2}
#' @param max_wait (optional) numeric vector of length 1. The max interval, in seconds, to wait between
#' iterations.
#' @param n Internal iteration counter used for recursion.
#' @param out Internal result accumulator
#'
#' @return
#' If > 1 iteration, a numeric vector equal in length to the number of starting queued reports,
#' where each element is a report ID, in order of processing, else \code{NULL}
#'
#' @note
#' This does not properly capture all completed reports, since there is a time delay between
#' the start of one and end of another iteration, and any reports that complete in that
#' interval will not be captured. This will be addressed shortly.
#'
#' For future improvements, it is likely this will move from a strict expotential growth delay
#' to an averaged time delta model of sorts, perhaps with lookup on the queue time projected by
#' the server versus actual times.
#'
#' At the moment, this is most useful for interactive sessions to denote when a set of queued reports
#' is complete.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' check_Queue()
#' check_Queue(base = 1.5) # for shorter time intervals between iterations
#' check_Queue(max_wait = 60) # cap interval time at 1 minute, regardless of iteration count
#' }
check_Queue <- function(x = NULL, base = 2, max_wait = NULL, n = 1L, out = data.frame()) {

  if(!is.null(x) && !is.integer(x)) {
    stop("x must be an integer or NULL")
  }

  if(!is.numeric(base)) {
    stop("If provided, 'base' must be a numeric")
  }

  stop_msg <- paste("Completed in", n, "iters")

  time_start <- Sys.time()
  start_queue <- GetQueue()

  if(!is.data.frame(start_queue)) {
    message(stop_msg)
    return(invisible(unique(out)))
  }

  curr_x <- start_queue[["reportID"]]
  if(is.null(x)) {
    x <- curr_x
  }

  diff_x <- intersect(curr_x, x)

  if(length(diff_x) == 0L) {
    message(
      "no additional values of x found in queue"
    )
    return(invisible(unique(out)))
  }

  diff_df <- data.frame(
    id = diff_x,
    completed = time_start,
    iter = n,
    stringsAsFactors = FALSE
  )

  start_queue <- start_queue[start_queue[["reportID"]] %in% diff_x, ]

  # console messages for interactive sessions
  message("----In iter ", n, ":----")
  message(nrow(start_queue), " remaining at current check")

  diff_len <- length(x) - length(diff_x)

  if(diff_len == 0L) {
    diff_len <- 0L
  }

  message(paste(diff_len, collapse = ", "), " finished since last check")

  # sleep timing
  next_check <- base[[1]]^n
  if(!is.null(max_wait) && is.numeric(max_wait)) {
    if(next_check > max_wait[[1]]) {
      next_check <- max_wait[[1]]
    }
  }

  message("Next check in ", sprintf("%.02f", next_check), " seconds...\n")

  Sys.sleep(next_check)

  check_Queue(x = diff_x, base = base, max_wait = max_wait, n = n + 1L,
               out = c(out, diff_x)
  )
}
