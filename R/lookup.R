
#' Lookup values from a lookup table
#'
#' @description
#' The [lookup()] function implements lookup of certain strings (such as
#' variable names) from a lookup table which maps keys onto values (such as
#' variable labels or descriptions). Original values are returned if they are
#' not found in the lookup table.
#'
#' The lookup table can be in the form of a two-column `data.frame`, in the form
#' of a named `vector`, or in the form of a `list`. If the table is in the form
#' of a `data.frame`, the lookup columns should be named `name` (for the key)
#' and `value` (for the value). If the lookup table is in the form of a named
#' `vector` or `list`, the name is used for the key, and the returned value is
#' taken from the values in the vector or list.
#'
#' Any names in x are not included in the result.
#'
#' @param x A string vector whose elements shall be looked up
#' @param lookup_table The lookup table to use.
#'
#' @return A string vector based on `x`, with values replaced with the lookup
#'   values from `lookup_table`. Any values not found in the lookup table are
#'   returned unchanged.
#'
#' @examples
#' fruit_lookup_vector <- c(a="Apple", b="Banana", c="Cherry")
#' lookup(letters[1:5], fruit_lookup_vector)
#'
#' mtcars_lookup_data_frame <- data.frame(
#'   name = c("mpg", "hp", "wt"),
#'   value = c("Miles/(US) gallon", "Gross horsepower", "Weight (1000 lbs)"))
#' lookup(names(mtcars), mtcars_lookup_data_frame)
#'
#' @md
#' @export
lookup <- function(x, lookup_table) {

  # If x is a factor, we look up the levels of x instead of the values of x
  # Note that levels of x will always be a character vector
  if (is.factor(x)) {
    levels(x) <- lookup(levels(x),lookup_table)
    return(x)
  }

  # Standardize the lookup_table
  lookup_table <- standardize_lookup_table(lookup_table)

  # Do the lookup
  result <- lookup_table[x]
  names(result) <- NULL
  not.found <- unlist(lapply(result, is.null))
  result[not.found] <- x[not.found]
  result <- unlist(result)

  # Return the result
  result
}

#' Construct lookup function based on a specific lookup table
#'
#' @description
#' The [lookuper()] function returns *a function* equivalent to the [lookup()]
#' function, except that instead of taking a lookup table as an argument, the
#' lookup table is embedded in the function itself.
#'
#' This can be very useful, in particular when using the lookup function as an
#' argument to other functions that expect a function which maps
#' `character`->`character` but do not offer a good way to pass additional
#' arguments to that function.
#'
#' @param lookup_table A the lookup table that should be used as the underlying
#'   lookup table for the returned function.
#'
#' @return A function that takes `character` vectors as its argument `x`, and
#'   returns either the corresponding values from the underlying lookup table,
#'   or the original values from x for those elements that are not found in the
#'   lookup table.
#'
#' @examples
#' lookup_fruits <- lookuper(list(a="Apple", b="Banana", c="Cherry"))
#' lookup_fruits(letters[1:5])
#'
#' @md
#' @export
lookuper <- function(lookup_table) {

  # Standardize the lookup_table
  lookup_table <- standardize_lookup_table(lookup_table)

  # Return a function suitable for lookups
  function(x) {
    lookup(x, lookup_table = lookup_table)
  }
}


#' Helper function to standardize the `lookup_table`.
#'
#' Preprocessing the lookup table to convert it to a list can take some time, so
#' when possible, we want to do it only once. Therefore we offload it to a
#' helper function
#'
#' @param lookup_table The unstandardized lookup table (must still be one of the
#'   formats specified for the `lookup()` function).
#'
#' @return The lookup table as a list.
#'
#' @md
#' @keywords internal
standardize_lookup_table <- function(lookup_table) {

  # Progressively convert data.frame -> vector -> list
  if (is.data.frame(lookup_table)) {
    stopifnot(
      "name" %in% names(lookup_table),
      "value" %in% names(lookup_table)
    )
    attributes(lookup_table$name) <- NULL
    attributes(lookup_table$value) <- NULL
    lookup_table <- tibble::deframe(lookup_table[, c("name", "value")])
  }
  if (is.vector(lookup_table) && !is.list(lookup_table)) {
    stopifnot(is.character(lookup_table))
    lookup_table <- as.list(lookup_table)
  }
  stopifnot(is.list(lookup_table))

  # Return the standardized lookup_table
  lookup_table
}

