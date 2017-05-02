#' Check current queue with messaging
#'
#' Automatically check the current queue at pre-determined time intervals
#'
#' @importFrom RSiteCatalyst GetQueue
#'
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
check_Queue <- function(base = 2, max_wait = NULL, n = 1L, out = c()) {

  if(!is.numeric(base)) {
    stop("If provided, 'base' must be a numeric")
  }

  stop_msg <- paste("Completed in", n, "iters")

  time_start <- Sys.time()
  start_queue <- GetQueue()

  if(!is.data.frame(start_queue)) {
    message(stop_msg)
    return(invisible(out[out != 0L]))
  }

  message("----In iter ", n, ":----")
  message(nrow(start_queue), " remaining at current check")

  next_check <- base[[1]]^n
  if(!is.null(max_wait) && is.numeric(max_wait)) {
    if(next_check > max_wait[[1]]) {
      next_check <- max_wait[[1]]
    }
  }

  message("Next check in ", sprintf("%.02f", next_check), " seconds...")

  if(is.data.frame(start_queue)) {
    Sys.sleep(next_check)
    curr_time  <- Sys.time()
    curr_queue <- GetQueue()
  } else {
    return(invisible(out[out!= 0L]))
  }

  delta <- setdiff(start_queue[["reportID"]], curr_queue[["reportID"]])

  if(length(delta) == 0L) {
    delta <- 0L
  }

  message(paste(delta, collapse = ", "), " finished in this check\n")

  check_Queue(base = base, max_wait = max_wait, n = n + 1L, out = c(out, delta))
}
