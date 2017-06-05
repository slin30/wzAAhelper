#' Make multiple argument lists from a single arglist
#'
#' For a specific named arg in a list of args, expand the latter based on list of specific args
#' @importFrom wzMisc depth
#' @param baseList named list of args. Must be unnested, i.e. single-level
#' @param targName chr vector of length 1 denoting what arg to modify, or if name is not found in \emph{baseList},
#'        what the added element should be named.
#' @param expandList list or vector of args to expand \code{baseList}. If a list, must be unnested, i.e. single-level.
#'        Must be (all) named or (all) unnamed. If named, \code{""} and \code{NA} are not allowed as names.
#' @param how \code{append} (default) or \code{replace} \emph{targName} value(s)?
#' @param ignore_depth (optional) length 1 logical. Bypass the depth (nesting) check? Useful in certain
#' circumstances.
#'
#' @details
#' Take a single list of baseline arguments and expand them based on a (named) list or vector of values.
#' Here, expand means that the input (baseline) list is replicated to length \emph{n},
#' where \code{n == length(expandList)}.
#'
#' By default, this function will never remove any elements or sub-elements (the latter if an element of
#' \emph{baseList} is of length >1). If an element of \emph{expandList} is all \code{NULL}, the
#' corresponding output will be unchanged for the default \emph{how} value of \code{append}.
#'
#' If \code{how = "replace"}, this output element denoted by \emph{targName} will contain only the
#' values in \emph{expandList}. In this event, a value of all \code{NULL} within an element of
#' \emph{expandList} will result in a missing element altogether in the respective output.
#'
#' When \code{how = "append"}, each replicated \emph{baseList} that is output is modified
#' based on the following rules:
#'
#' \itemize{
#' \item If \emph{targName} is a name in \emph{baseList}, the corresponding element of
#'       \emph{baseList} is modified using \link[purrr]{update_list}. This is done \emph{i}
#'       times, for each \code{expandList[i]}, such that each return list second-level element,
#'       \code{return_list[[i]]}, will contain the same number of elements as the original elements
#'       within \emph{baseList}.
#' \itemize{
#' \item Each \code{return_list[[i]][[targName]]} will (may) be modified according to
#'       the corresponding value in \emph{expandList}. The only time no modification will occur
#'       is if a vector of \code{NULL}s is present for the i-\emph{th} element of \emph{expandList}.
#'        See notes.
#'        }
#' \item If \emph{targName} is not a name in \emph{baseList}, an element of \emph{baseList} is added
#'       using \link[purrr]{update_list} with the name provided in \emph{targName}, with a warning message.
#'       This is not the primary intended use for this function, but it is a valid application. See notes.
#' }
#'
#' For \code{how = "replace"}, the same general rules apply, except explicit replacement will occur, or in
#' the event one or more elements within \emph{expandList} is all \code{NULL}, the corresponding output
#' (argument) will be dropped (as mentioned previously).
#'
#' @note
#' The most typical use case is when you have a list of common baseline arguments to
#' an API call (function) and wish to create multiple lists of arguments using the baseline arglist as a
#' "template," where each version differs by one or more values within a single argument. You must
#' ensure that the API call will support a vector of values > length 1 for \emph{targName}; this is
#' particularly important when used with the default \code{how = "append"} option.
#'
#' It is also possible to use this function to add a new (named) argument with zero or more values to
#' a baseline arglist, although you might be better served with something much simpler, such as
#' \link[purrr]{update_list}, e.g.
#' \itemize{
#' \item \code{lapply(expandList, function(f) update_list(baseList, targName = f))}
#' }
#' The above approach is also useful if you wish to simply replace, and not append, values of an existing
#' arg (although you could also run this function with \code{how = "replace"}).
#'
#' If you simply wish to append an additional arg to the output, either add it to the \emph{baseList} or
#' try:
#' \itemize{
#' \item \code{map(output_list, update_list, enqueueOnly = TRUE)}
#' }
#' Which will add the \code{enqueueOnly = TRUE} arg to each arglist within \code{output_list}.
#'
#' @return
#' A named nested list of length \emph{expandList}, with each first-level element containing the elements
#' of \emph{baseList}, modified as defined by \emph{targName} and \emph{expandList}.
#' Names are taken from the names of \emph{expandList} if provided, otherwise will use values as names,
#' uniquified via \code{\link{make.unique}} if necessary.
#'
#' IMPORTANT: Your output list of args may not be immediately usable with API calls. This if usually easy
#' to resolve with \link[purrr]{lift}, specifically \link[purrr]{lift_dl}, e.g.
#' \itemize{
#' \item \code{lifted_fun <- purrr::lift_dl(RSiteCatalyst::QueueTrended)}
#' }
#'
#' Alternatively use \code{\link{do.call}}.
#'
#' @export
#' @examples
#' #define a baselist
#' my_baseList <- list(
#' reportsuite.id = "myID",
#' date.from = "2016-09-01",
#' date.to = "2016-09-02",
#' elements = "my_element",
#' metrics = c("pageviews", "visits"),
#' segment.id = c("segment1", "segment2"),
#' date.granularity = "month",
#' top = 10)
#'
#' my_expandList <- list(
#' seg1 = "a",
#' seg2 = "b",
#' seg3 = "c",
#' seg4 = "d")
#'
#' #Expand segment.id element, default append option
#' lst_expanded <- expand_argList(my_baseList, "segment.id", my_expandList)
#'
#' #Clearer to see this way:
#' purrr::transpose(lst_expanded)[["segment.id"]]
#'
#' #Replace segment.id element
#' lst_expanded_replace <- expand_argList(my_baseList, "segment.id", my_expandList, how = "replace")
#' purrr::transpose(lst_expanded_replace)[["segment.id"]]
expand_argList <- function(baseList, targName, expandList, how = c("append", "replace"), ignore_depth = FALSE) {

  if(!is.list(expandList)) {
    expandList <- as.list(expandList)
  }

  if(ignore_depth == FALSE && (depth(baseList) != 1 | depth(expandList) != 1)) {
    stop("baseList and expandList must both be unnested lists, i.e. of depth 1")
  }
  if(length(targName) != 1) {
    stop("targName argument must be of length 1")
  }
  if(any(names(expandList) == "") | anyNA(names(expandList))) {
    stop("\r", substitute(expandList),
         " contains unnamed elements.\r\n",
         "All elements of targName must be named to avoid ambiguous output")
  }
  if(all(is.null(names(expandList)))) {
    val_uniquecheck <- anyDuplicated(expandList)
    if(val_uniquecheck == FALSE) {
      message("expandList did not contain any names but all values are unique.\r\n",
              "Using values of expandList as names")
      names(expandList) <- expandList
    } else {
      message("expandList did not contain any names and there are duplicate values.\r\n",
              "Using values of expandList as names via make.unique")
      names(expandList) <- make.unique(unlist(expandList))
    }
  }
  if(any(names(baseList) == "") | anyNA(names(baseList))) {
    warning(substitute(baseList), " elements are not all named")
  }
  if(!targName %in% names(baseList)) {
    warning(targName, " not found in ", substitute(baseList),
            "\rAdding '", targName, "' as an additional argument")
  }


  if(missing(how)) {
    how <- "append"
  }

  targ <- baseList[[targName]]
  if(how == "append") {
    multi_targ <- lapply(expandList, function(f) c(f, targ))
  } else {
    multi_targ <- lapply(expandList, function(f) c(f))
  }

  tmp <- lapply(multi_targ, function(f) purrr::update_list(
    baseList, targName = f
  ))

  filt    <- Map(.helper_nameFilt, x = tmp, targName = targName)
  tmp_sub <- Map("[", tmp, filt)

  lapply(tmp_sub, function(f) .helper_nameSet(f, targName))

}



.helper_nameFilt <- function(x, targName) names(x) != targName

.helper_nameSet  <- function(x, targName) {
  nms <- names(x)
  nms[nms == "targName"] <- targName
  nms
  purrr::set_names(x, nms)
}


