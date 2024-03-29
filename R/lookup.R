
#' Lookup values from a lookup table
#'
#' @description
#' The [lookup()] function implements lookup of certain strings (such as
#' variable names) from a lookup table which maps keys onto values (such as
#' variable labels or descriptions).
#'
#' The lookup table can be in the form of a two-column `data.frame`, in the form
#' of a named `vector`, or in the form of a `list`. If the table is in the form
#' of a `data.frame`, the lookup columns should be named `name` (for the key)
#' and `value` (for the value). If the lookup table is in the form of a named
#' `vector` or `list`, the name is used for the key, and the returned value is
#' taken from the values in the vector or list.
#'
#' Original values are returned if they are not found in the lookup table.
#' Alternatively, a `default` can be specified for values that are not found.
#' Note that an `NA` in x will never be found and will be replaced with the
#' default value. To specify different defaults for values that are not found
#' and for `NA` values in `x`, the `default` must be crafted manually to achieve
#' this.
#'
#' Any names in x are not included in the result.
#'
#' @param x A string vector whose elements are to be looked up.
#'
#' @param lookup_table The lookup table to use.
#'
#' @param default If a value is not found in the lookup table, the value will be
#'   taken from `default`. This must be a character vector of length 1 or the
#'   same length as x. Useful values include `x` (the default setting), `NA`, or
#'   `""` (an empty string).
#'
#' @return The [lookup()] function returns string vector based on `x`, with
#'   values replaced with the lookup values from `lookup_table`. Any values not
#'   found in the lookup table are taken from `default`.
#'
#' @examples
#' fruit_lookup_vector <- c(a="Apple", b="Banana", c="Cherry")
#' lookup(letters[1:5], fruit_lookup_vector)
#' lookup(letters[1:5], fruit_lookup_vector, default = NA)
#'
#' mtcars_lookup_data_frame <- data.frame(
#'   name = c("mpg", "hp", "wt"),
#'   value = c("Miles/(US) gallon", "Gross horsepower", "Weight (1000 lbs)"))
#' lookup(names(mtcars), mtcars_lookup_data_frame)
#'
#' @export
lookup <- function(x, lookup_table, default = x) {

  # Check args (lookup table is checked separately)
  is.character(x)       || is.factor(x)        || stop("x must be a character or a factor")
  is.character(default) || all(is.na(default)) || stop("default must be a character vector or NA")
  length(default) %in% c(1, length(x))         || stop("length(default) must be 1 or length(x)")

  # If x is a factor, we look up the levels of x instead of the values of x
  # Note that levels of x will always be a character vector
  if (is.factor(x)) {
    levels(x) <- lookup(levels(x), lookup_table)
    return(x)
  }

  # Standardize the lookup_table
  lookup_table <- standardize_lookup_table(lookup_table)

  # Do the lookup
  result <- lookup_table[x]
  names(result) <- NULL
  not.found <- unlist(lapply(result, is.null))
  if (length(default) == 1) {
    result[not.found] <- default
  } else {
    result[not.found] <- default[not.found]
  }
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
#' @return The [lookuper()] function returns *a function* that takes `character`
#'   vectors as its argument `x`, and returns either the corresponding values
#'   from the underlying lookup table, or the original values from x for those
#'   elements that are not found in the lookup table (or looks them up from the
#'   `default`).
#'
#' @examples
#' lookup_fruits <- lookuper(list(a="Apple", b="Banana", c="Cherry"))
#' lookup_fruits(letters[1:5])
#'
#' @rdname lookup
#' @export
lookuper <- function(lookup_table, default = NULL) {

  # Standardize the lookup_table
  lookup_table <- standardize_lookup_table(lookup_table)

  # Return a function suitable for lookups
  if (is.null(default)) {
    result <- function(x) {
      lookup(x, lookup_table = lookup_table)
    }
  } else {
    result <- function(x) {
      lookup(x, lookup_table = lookup_table, default = default)
    }
  }

  # Return result
  result
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

