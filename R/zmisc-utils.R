
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
#' @keywords internal
#' @export
wrap_error <- function(e, wrap=50) {
  cat( paste0("#E> ", strwrap(e$message, width=50), "\n"), sep="" )
  invisible(e)
}


#' Apply a function to each column of a data.frame
#'
#' Thin wrapper around `lapply()` that checks that the input is a table before
#' applying the function to each column, and converts the result back to a table
#' afterwards. If the `tibble` package is available and the input is a `tibble`,
#' the result will be a `tibble`; otherwise, it will be a plain `data.frame`.
#'
#' @param d A `data.frame` or `tibble`.
#' @param fun A function to apply to each column of `d`.
#'
#' @return A `data.frame` or `tibble` with the function applied to each column.
#'
#' @examples
#' df <- data.frame(
#'   col1 = c(1, 2, 3),
#'   col2 = c(4, 5, 6)
#' )
#' sum_fun <- function(x) sum(x)
#' result <- ddply_helper(df, sum_fun)
#' print(result)
#'
#' @keywords internal
#' @export
ddply_helper <- function(d, fun) {

  # Check args
  is.data.frame(d) || stop("d must be a data.frame")

  # Apply the function
  result <- lapply(d, fun)

  # Convert result to a table
  if (inherits(d, "tbl_df") && requireNamespace("tibble", quietly = TRUE)) {
    # Convert to tibble if input is a tibble and tibble package is available
    result <- tibble::as_tibble(result)
  } else {
    # Else convert to a plain data.frame
    result <- as.data.frame(result)
  }

  # Return the result
  result
}
