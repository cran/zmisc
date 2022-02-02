
#' Utility function to output an error
#'
#' This function is used to capture errors, typically inside a `tryCatch()`
#' statement and output them in a clean and readable way. The function provides
#' line-wrapping, with a configurable width. When printing the error message, it
#' prefixes the text with "`#E> `" to make it easier to look for the error.
#'
#' @param e The error to wrap.
#' @param wrap How many characters per line before wrapping.
#'
#' @return The original error is returned invisibly.
#'
#' @examples
#' tryCatch(stop("This is an error"), error=wrap_error)
#'
#' @md
#' @export
#' @keywords internal
wrap_error <- function(e, wrap=50) {
  cat( paste0("#E> ", strwrap(e$message, width=50), "\n"), sep="" )
  invisible(e)
}

