
#' Generate sequence in a safe way
#'
#' @description
#' The [zeq()] function creates an increasing integer sequence, but differs from
#' the standard one in that it will not silently generate a decreasing sequence
#' when the second argument is smaller than the first. If the second argument is
#' one smaller than the first it will generate an empty sequence, if the
#' difference is greater, the function will throw an error.
#'
#' @param from The lower bound of the sequence
#' @param to   The higher bound of the sequence
#'
#' @return A sequence ranging from `from` to `to`
#'
#' @examples
#' # For increasing sequences, zeq() and seq() are identical
#' zeq(11,15)
#' zeq(11,11)
#'
#' # If second argument equals first-1, an empty sequence is returned
#' zeq(11,10)
#'
#' # If second argument is less than first-1, the function throws an error
#' tryCatch(zeq(11,9), error=wrap_error)
#'
#' @md
#' @export
zeq = function(from, to)
{
    stopifnot ( round(from) == from )
    stopifnot ( round(to)   == to   )
    stopifnot ( to >= from - 1      )
    return (seq_len(1+to-from)+from-1)
}

