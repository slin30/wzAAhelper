#' Batch enqueue and pull RSiteCatalyst Queue* reports
#'
#' For a list of arguments in the scope of a Queue* function, enqueue and pull data
#'
#' @importFrom RSiteCatalyst QueueRanked QueueTrended QueuePathing QueueFallout
#' QueueOvertime QueueSummary QueueDataWarehouse GetReport
#'
#' @param argList (required) A (nested) list of named arguments to pass to \emph{fun}
#' @param fun (optional) A \code{quoted} character vector of length 1. Defaults to
#' \link[RSiteCatalyst]{QueueRanked}.
#' @param use_names (optional) A logical vector of length 1. Should the names of
#' \emph{argList} be automatically appended to the output result? Defaults to
#' \code{TRUE} and igored in-function if no or missing names detected
#' @param auto_enqueue (optional) A logical vector of length 1. Should we append an
#' argument of \code{enqueueOnly} and set the value to \code{TRUE} if this argument
#' is not detected in all element-lists of \emph{argList}? Defaults to \code{TRUE}.
#' @param enqueue_only (optional) A logical vector of length 1. Only enqueue, i.e.
#' do not attempt to also pull the report? Defaults to \code{FALSE}.
#' @param ... (optional) Additional arguments to pass to \link[RSiteCatalyst]{GetReport}
#'
#' @return
#' A list the same length of \emph{argList} containing either the \code{data.frame} output from
#' \code{GetReport} or the (\code{integer}) report ID. The latter applies when an error is
#' encountered while trying to pull the report, which almost always happens because
#' one or more submitted reports requires additional processing time.
#'
#' If \code{use_names = TRUE} and all names within \emph{argList} are
#' present, the output elements will be named using the corresponding (positional) names
#' of \emph{argList}.
#'
#' Each \code{data.frame} element will have an attribute called \code{reportID}
#' to denote the ID from each respective enqueued report, in the event
#' you wish to e.g. re-pull one or more reports. \code{integer} elements (i.e. report IDs only)
#' will not contain attributes, since this would be redundant (and potentially confusing).
#'
#' @details
#' The list of valid arguments for \emph{fun} includes all \code{RSiteCatalyst}
#' Queue\code{*} functions where \emph{enqueueOnly} is a valid argument. This
#' currently includes:
#'
#' \itemize{
#' \item QueueRanked
#' \item QueueTrended
#' \item QueuePathing
#' \item QueueFallout
#' \item QueueOvertime
#' \item QueueSummary
#' \item QueueDataWarehouse
#' }
#'
#' @export
#'
#' @examples
#' #TBD
enqueue_batch <- function(argList, fun = "QueueRanked", use_names = TRUE,
                          auto_enqueue = TRUE, enqueue_only = FALSE, ...)
  {

  lstNms <- names(argList)
  lstLEN <- length(argList)

  # input structure check
  if(wzMisc::depth(argList) < 2L) {
    stop("Expected a nested list of depth >= 2, but depth of argList is ",
         wzMisc::depth(argList))
  }

  if(length(fun) > 1L) {
    stop("'fun' must be a character vector of length 1")
  }

  if(is.null(lstNms) || length(lstNms) != lstLEN) {
    use_names <- FALSE
  }

  funList <- list(
    QueueRanked = RSiteCatalyst::QueueRanked,
    QueueTrended = RSiteCatalyst::QueueTrended,
    QueuePathing = RSiteCatalyst::QueuePathing,
    QueueFallout = RSiteCatalyst::QueueFallout,
    QueueOvertime = RSiteCatalyst::QueueOvertime,
    QueueSummary = RSiteCatalyst::QueueSummary,
    QueueDataWarehouse = RSiteCatalyst::QueueDataWarehouse
  )

  # check that fun is valid
  funOut <- funList[[fun]]
  if(is.null(funOut)) {
    stop("'fun' must be one of ",
         paste(names(funList), collapse = ", "))
  }

  # check that enqueueOnly is present, maybe auto-append TRUE
  enqueue_check <- Map("[[", argList, "enqueueOnly")

  if(length(enqueue_check) != lstLEN || sum(unlist(enqueue_check)) != lstLEN) {
    if(!auto_enqueue) {
      stop(paste("Not all elements of argList contain an argument of enqueueOnly",
             "  with value TRUE, and auto_enqueue is FALSE", sep = "\n")
      )
    } else {
      message("Adding enqueueOnly with value of TRUE to argList")
      argList <- Map(c, argList, enqueueOnly = TRUE)
    }
  }

  lst_nq <- vector("list", lstLEN)
  for(i in seq_along(argList)) {
    message("Enqueueing ", i, " of ", lstLEN)
    lst_nq[[i]] <- as.integer(
      do.call(funOut, argList[[i]])
    )
  }

  # early return if enqueue_only is TRUE
  if(enqueue_only) {
    if(use_names) {
      names(lst_nq) <- lstNms
    }
    return(lst_nq)
  }

  # check that length of enqueue list is OK
  nqLEN <- length(
    Filter(function(x) !is.null(x), lst_nq)
  )
  if(lstLEN != nqLEN) {
    stop("Difference in lengths of enqueued vs. input!")
  }

  lst_rp <- vector("list", nqLEN)
  for(i in seq_along(lst_nq)) {
    message("Pulling ", i, " of ", nqLEN)
    lst_rp[[i]] <- tryCatch(
      {
        GetReport(lst_nq[[i]], ...)
      },
      error = function(e) {
        message(e, " ...Returning report ID instead")
        lst_nq[[i]]
      }
    )
  }

  if(use_names) {
    names(lst_rp) <- lstNms
  }

  # set attr on output list
  is_id <- vapply(lst_rp, function(f) is.integer(f), logical(1))

  if(all(is_id)) {
    return(lst_rp)
  }

  lst_rp[!is_id] <- Map(`attr<-`, lst_rp[!is_id], "reportID", lst_nq[!is_id])

  return(lst_rp)
}
