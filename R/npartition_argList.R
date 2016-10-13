#' Partition a single top arg into n chunks
#'
#' Take a base argument list with a "top" arg and partition into n chunks
#'
#' @importFrom wzMisc depth
#' @importFrom wzMisc make_chunks
#'
#' @param baseList named list of args. Must be unnested, i.e. single-level
#' @param n integer of length 1. Number of partitions to make. \code{numeric} will be coerced via \code{as.integer}
#' @param chunkLimit optional. Should almost never be used. Controls the upper limit of chunk size for
#' \link[wzMisc]{make_chunks}. Pre-set to 50K and should be left as-is.
#'
#' @details
#' There are three distinct uses for this function; the first two are the most important:
#' \itemize{
#' \item You have more than \code{50000} results and need to break your calls up to pull all available results.
#' Most often, this scenario is detected when you have a query return 50000 records, which is almost always
#' indicative of hitting the default 50K limit.
#' \item You have nested elements, e.g. you wish to break down \emph{n} products by \emph{p} types,
#' where you anticipate that the resulting \code{n x p} result is likely to exceed 50K. If you are (also) running a
#' time-series report, this is even more problematic.
#' \item You are pulling tens of thousands of data points, but expect that you will be under the 50K limit by
#' a safe margin. It is often inefficient to request (tens of) thousands of records in a single call, and
#' here, this function can be useful to simply reduce wait time.
#' }
#'
#' @note
#' This function calls \code{\link{expand_argList}}, so please refer to the documentation there for additional
#' caveats around using the returned results. If you wish to concomitantly use this function with \code{expand_arglist},
#' do so with a call to \code{lapply} or \code{Map} or \link[purrr]{map}.
#'
#' Alternatively, an explicit \code{for} loop is useful as a means to handle the highest (outermost)
#' level of nesting, as this is usually clearer than creating nested lists of depth >= 3.
#'
#' @return
#' A nested list of length equal to \code{(baseList[["top"]]/n) + (baseList[["top"]] \%\% n)}.
#' If not present, an argument of "start" will be appended to enable data fetching indexing.
#'
#' If \code{length(baseList[["top"]]) > 1} is \code{TRUE}, the resulting chunks will preserve
#' values beyond the first. The "start" argument for each chunk will always be of length 1, as the
#' API will only apply "start" to the first element of "top."
#'
#' It is helpful to use the outputs in conjunction with the \code{enqueueOnly=TRUE} argument most
#' \code{\link[RSiteCatalyst]{RSiteCatalyst}} call constructors support, not only for
#' performance, but also general organization.
#' @export
#'
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
#' top = 100000) #exceeds 50K limit!
#'
#' npartition_argList(my_baseList, n = 2)
#' npartition_argList(my_baseList, n = 3) #not very efficient, see:
#' unlist(Map("[", npartition_argList(my_baseList, n = 3), "top"))
#' npartition_argList(my_baseList, n = 4)
#'
#' #with top values >1L
#' my_baseList_2 <- list(
#' reportsuite.id = "myID",
#' date.from = "2016-09-01",
#' date.to = "2016-09-02",
#' elements = c("my_element_1", "my_element_2"),
#' metrics = c("pageviews", "visits"),
#' segment.id = c("segment1", "segment2"),
#' date.granularity = "month",
#' top = c(100000, 10))
#'
#' npartition_argList(my_baseList_2, n = 2)
#'
#' #Error if length top != length elements, if length top > 1 OR length elements > 1
#' \dontrun{
#'
#' # length 2 top, length 1 elements
#' bad_list_1 <- list(
#' reportsuite.id = "myID",
#' date.from = "2016-09-01",
#' date.to = "2016-09-02",
#' elements = c("my_element_1"),
#' metrics = c("pageviews", "visits"),
#' date.granularity = "month",
#' top = c(100000, 10))
#'
#' npartition_argList(bad_list_1, n = 2)
#'
#' # length 1 top, length 2 elements
#' bad_list_2 <- list(
#' reportsuite.id = "myID",
#' date.from = "2016-09-01",
#' date.to = "2016-09-02",
#' elements = c("my_element_1", "my_element_2"),
#' metrics = c("pageviews", "visits"),
#' date.granularity = "month",
#' top = c(100000))
#'
#' npartition_argList(bad_list_2, n = 2)
#' }
npartition_argList <- function(baseList, n, chunkLimit = NULL) {
  if(is.null(chunkLimit)) {
    chunkLimit <- 5E4
  }

  if(depth(baseList) != 1) {
    stop("baseList must be a non-nested list, i.e. of depth 1")
  }

  if(is.null(baseList[["top"]])) {
    stop("A non-null value for the named argument 'top' must be present in baseList")
  }

  topLen    <- length(baseList[["top"]])
  elemCheck <- is.null(baseList$element)
  elemLen <- if(!elemCheck) {
    length(baseList$element)
  } else NULL

  #warn if topLen is > length of elements, if topLen > 1
  if(topLen > 1 && is.null(elemLen)) {
    warning(paste(strwrap("Length of 'top' argument is > 1 but element(s) arg is
                          missing. 'top' arguments > length 1 are usually valid only when
                          there are element(s) arguments of equal length",
                          width = 80, indent = 0, exdent = 2),
                  collapse = "\n")
    )
  }

  if(!is.null(elemLen))
    if(elemLen != topLen) {
      stop(paste(strwrap(
        "Length of 'top' argument is not equal to length of
        'element' argument. Standard recycling rules may
        result in ambiguity when partitioning in such scenarios
        Please supply a vector of 'top' equal in length to 'element(s)'",
        width = 80, indent = 0, exdent = 2),
        collapse = "\n")
      )
    }

  if(length(n) != 1) {
    stop("n must be of length 1")
  }

  tot <- as.integer(baseList[["top"]][[1]])
  #generate chunk df
  chunks <- make_chunks(tot, chunk_size = tot/n, limit = chunkLimit)

  start_xpandChunk <- as.list(chunks[["from"]])
  top_xpandChunk   <- as.list(chunks[["size"]])

  #capture additional, append to top_xpandChunk
  addtl <- as.integer(baseList[["top"]][-1])
  top_xpandChunk <- lapply(top_xpandChunk, function(f) c(f, addtl))

  chunk_names <- paste0("chunk", seq_len(nrow(chunks)))

  names(start_xpandChunk) <- chunk_names
  names(top_xpandChunk)   <- chunk_names

  xpanded_lst <- expand_argList(baseList, targName = "start",
                                start_xpandChunk, how = "replace")
  Map(purrr::update_list, xpanded_lst, top = top_xpandChunk)

}
