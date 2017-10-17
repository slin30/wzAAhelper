#' Get account ids by account name clean
#'
#' Efficiently run a breakdown report, of account ids by account name clean
#'
#' @importFrom RSiteCatalyst QueueRanked GetClassifications
#'
#' @param ... (required) Named args to pass to fun. Accepts bare named elements, a vector of named elements,
#' or a list of named elements.
#' @param fun (optional) A quoted function; defaults to \code{QueueRanked} and generally should be
#' left as such. Not used if \code{construct_only = TRUE}.
#' @param chunk_length (optional) An integer vector of length 1 denoting the number of rows to pull for
#' each chunk, based on the first (parent) element (\emph{Account Name}). Defaults to \code{200}.
#' @param n_ids (optional) An integer vector of length 1 denoting the number of child elements to pull for each
#' parent element. Defaults to \code{10}.
#' @param construct_only (optional) A logical vector of length 1 denoting if the function should only output the
#' argument lists, and not actually enqueue the server
#'
#' @return
#' If \code{construct_only = FALSE} (default), a list of report IDs that can be passed to
#' \link[RSiteCatalyst]{GetReport}. Otherwise, a nested list of arguments that can be
#' passed to the appropriate function of your choice.
#'
#' @details
#' The purpose of this function is to streamline the otherwise lengthy process of pulling down
#' account ids by account name (clean). This function does so by chunking the call across
#' the total number of account names (possibly clean) into \emph{chunk_length}-long pieces, i.e. \code{200}-
#' row requests (by default).
#'
#' \code{...} should consist of your basic query parameters, and the following must be present:
#'
#' \itemize{
#' \item \code{reportsuite.id}
#' \item \code{date.from}
#' \item \code{date.to}
#' \item \code{metrics}
#' }
#'
#' Additional arguments accepted by \code{fun} are passed-through, with the \strong{exception} of:
#'
#' \itemize{
#' \item \code{elements}
#' \item \code{classification}
#' }
#'
#' If either of these two named elements are detected, they will be (silently) removed and replaced
#' with the default arguments, namely
#'
#' \itemize{
#' \item \code{elements = c("evar7", "prop1")}
#' \item \code{classification = c("Account Name [v2] Clean", "")}
#' }
#'
#' If your report suite does not have the default first (only) classification of \code{Account Name [v2] Clean},
#' the value is set to blank i.e. \code{""}. This is handled automatically by a call to
#' \link[RSiteCatalyst]{GetClassifications}.
#'
#' Note that if you pass any arguments of \code{fun} that are vectors of length > 1, you must do so
#' as a \code{list}, else the key-value structure will be corrputed by \code{c()}, which is called on
#' args passed to \code{...}.
#'
#' Finally, if an argument of \code{top} is not included, this function will call \code{fun} along with
#' a calculated metric, id \code{cm300000520_57bbb21bec83505f3c442b1d}, to determine the maximum number of
#' account names (clean) according to your other input parameters (passed in via \code{...}). This requires
#' that you have access to said calculated metric, and takes a bit of extra time, in exchange for convenience.
#' Including a named element of \code{top} with a valid value bypasses this step (and its requirements).
#'
#'
#' @note
#' You may wish to experiment with the threshold for \emph{chunk_length} to see if e.g. lower values improve
#' performance. This is often the case, within reason.
#'
#' Even with a reasonably low \emph{chunk_length} (which \code{200} is), you should still check the Queue via
#' \link[RSiteCatalyst]{GetReport} or \link{check_Queue} to ensure all your reports have processed prior to
#' requesting them. You should expect to wait more than 5 minutes, even when using this approach.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## Offline-compatible; API access not needed for these two examples
#' # Only generate an argument list, and do not call the API to get 'top'
#' enqueue_IdByAccount(
#'     date.from = "2016-01-01", date.to = Sys.Date(),
#'     metrics = "visits",
#'     reportsuite.id = "elsevier-rx-prod",
#'     top = 2063,
#'     construct_only = TRUE
#' )
#' # Only generate an argument list, and do not call the API to get 'top'
#' # Note that you must use a list if you need to pass in args
#' #  to ... that are of length > 1!
#' enqueue_IdByAccount(
#'     list(date.from = "2016-01-01", date.to = Sys.Date(),
#'     metrics = c("visits", "uniquevisitors"),
#'     segment.id = c("segment1", "segment2"),
#'     reportsuite.id = "elsevier-rx-prod",
#'     top = 2063),
#'  construct_only = TRUE
#' )
#'
#' ## API access needed for these examples
#' # Only generate an argument list and call the API to get 'top'
#' enqueue_IdByAccount(
#'     date.from = "2016-01-01", date.to = Sys.Date(),
#'     metrics = "visits",
#'     reportsuite.id = "elsevier-rx-prod"
#' )
#' # Enqueue reports and call the API to get 'top'
#' enqueue_IdByAccount(
#'     date.from = "2016-01-01", date.to = Sys.Date(),
#'     metrics = "visits",
#'     reportsuite.id = "elsevier-rx-prod"
#' )
#' }
enqueue_IdByAccount <- function(..., fun = "QueueRanked",
                                chunk_length = 200L, n_ids = 10L,
                                construct_only = FALSE) {

  args_coll <- c(...)

  if(is.list(args_coll)) {
    args_base <- args_coll
  } else if (is.atomic(args_coll) && length(args_coll) > 1L) {
    args_base <- as.list(args_coll)
  } else {
    args_base <- list(...)
  }

  # if any named elements within static names present, remove them
  static_nms <- c("elements", "classification")
  if(any(static_nms %in% names(args_base))) {
    args_base <- args_base[! names(args_base) %in% static_nms]
  }

  req_nms <- c(
    "date.from", "date.to",
    "reportsuite.id",
    "metrics"
  )
  opt_nms <- setdiff(names(formals(fun)), req_nms)

  if(! all(req_nms %in% names(args_base))) {
    stop("Not all required named elements provided")
  }

  if(! all(opt_nms %in% names(formals(fun)))) {
    stop("Unexpected named element(s)")
  }

  args_base[c("date.from", "date.to")] <- .fix_date(args_base[c("date.from", "date.to")])

  # define static elements
  el_1 <- "evar7"
  el_2 <- "prop1"

  cs_1 <- "Account Name [v2] Clean"
  cs_2 <- ""

  # check if we can use classification at all
  class_exists <- .check_classification(rsid = args_base[["reportsuite.id"]],
                                        element = el_1)
  if(!class_exists) {
    cs_1 <- ""
  }

  # create static elements-class list
  statics <- list(
    elements = c(el_1, el_2),
    classification = c(cs_1, cs_2)
  )

  # handle top for first element, use if provided, else pull fresh
  if("top" %in% names(args_base)) {
    top_first <- args_base[["top"]][[1]]
    args_base[["top"]] <- c(top_first, n_ids)
  } else {
    top_check_report <- do.call(
      match.fun(fun),
      c(args_base[c("date.from", "date.to", "reportsuite.id")],
        list(metrics = c("visits",
                         "cm300000520_57bbb21bec83505f3c442b1d"),
             elements = statics[["elements"]][[1]],
             classification = statics[["classification"]][[1]],
             top = 1,
             validate = FALSE
        )
      )
    )
    top_check <- top_check_report[["cm300000520_57bbb21bec83505f3c442b1d"]]
    statics[["top"]] <- c(top_check, n_ids)
  }

  # complete arglist
  full_argList <- c(
    args_base,
    statics
  )
  # prevent args passed to ... from becoming part of query
  fun_args <- names(formals(fun))
  full_argList <- full_argList[intersect(fun_args, names(full_argList))]

  n_lim <- ceiling(as.integer(full_argList[["top"]][[1]])/chunk_length)

  # partition and enqueue
  part_argList <- npartition_argList(full_argList, n = n_lim)

  if(construct_only) {
    return(part_argList)
  }

  enqueue_batch(part_argList,
                fun = fun,
                enqueue_only = TRUE
  )

}

NULL
.fix_date <- function(x) {

  ok <- grepl("\\d{4}-\\d{2}-\\d{2}", x, perl = TRUE)
  to_fix <- x[!ok]

  fixed <- paste0(as.Date(as.integer(to_fix), origin = "1970-01-01"))

  x[!ok] <- fixed
  x
}

NULL
.check_classification <- function(rsid, element) {

  if(length(element) > 1L) {
    stop("element must be a character vector of length 1")
  }

  out <- tryCatch(
    {
      GetClassifications(rsid, element)
    },
    error = function(e) {
      invisible(e)
      NULL
    },
    finally = NULL
  )

  !is.null(out)
}
