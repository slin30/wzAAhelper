#' String normalization for common account name formats
#'
#' Strip out certain specials, make lowercase, optionally parse and handle markup and encoding
#'
#' @importFrom wzMisc unescape_markup
#' @importFrom stringr str_match str_trim
#'
#' @param x (required) A character vector to normalize
#' @param strip_prefix (optional) Should all characters up to and including \code{:} be stripped?
#' Defaults to \code{TRUE}
#' @param fix_markup (optional) Should html-markup be escaped? Defaults to \code{TRUE}; and
#' calls \code{\link[wzMisc]{unescape_markup}} with fixed parameters; see details.
#' @param fix_encoding (optional) Should encoding be normalized? Defaults to \code{TRUE} and
#' calls \code{\link[base]{iconv}} with fixed parameters; see details
#'
#' @return
#' A normalized vector. With default args, this means the following:
#'
#' \enumerate{
#' \item html markup is stripped, leaving the values intact
#' \item encoding is set to \code{ASCII}, with non-translatable characters removed
#' \item only text following the first \code{:} (colon) is preserved, i.e. if any text is
#'       prefixed with \emph{ae:}, only text after \emph{ae:} will be kept
#' \item all punctuation is stripped
#' \item all slashes (forward and back) are stripped
#' \item all whitespace is normalized, i.s. whitespace runs of length > 1 are converted to single-length runs
#' \item leading and trailing whitespace is stripped
#' \item all case is transformed to lowercase
#' }
#'
#' @details
#' TBD
#'
#' @export
#'
#' @examples
#' # TBD
normalize_actName <- function(x, strip_prefix = TRUE, fix_markup = TRUE, fix_encoding = TRUE) {

  # auto-fix options
  if(fix_markup & !fix_encoding) {
    x <- unescape_markup(x, what_ml = "html")
  }
  if(!fix_markup & fix_encoding) {
    x <- iconv(x, from = "UTF-8", to = "ASCII", sub = "")
  }
  if(fix_markup & fix_encoding) {
    x <- unescape_markup(x, what_ml = "html", iconv_encoding = TRUE,
                         from = "UTF-8", to = "ASCII",
                         sub = "")
  }


  # extract out prefix if present
  if(strip_prefix) {
    x <- str_match(x,  "(?>\\w+:)?(.*$)")[, 2]
  }

  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("\\\\", "", x)
  x <- gsub("/", "", x)
  x <- gsub("\\s{2,}", " ", x)

  x <- tolower(str_trim(x))

}
