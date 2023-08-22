
#' Return a threadbare version of a vector
#'
#' A bare object is an R object that has no class attributes (see
#' [rlang::is_bare_character()]). A threadbare object is an atomic object (i.e.
#' not a [list()], see [is.atomic()]), with no attributes at all. The function
#' returns an error if a list is passed.
#'
#' @param x A vector, possibly classed, but not a list object, to strip of all
#'   attributes.
#'
#' @return A vector with the same core values as `x`, but with no [attributes()]
#'   at all, not even [names()].
#'
#' @family labelled light
#' @keywords internal
threadbare <- function(x) {
  is.atomic(x) || stop('x must be atomic')
  attributes(x) <- NULL
  x
}

#' Create a labelled variable
#'
#' @description
#'
#' The labelled_light (ll) collection is a minimal implementation of core
#' functions for creating and managing `haven_labelled` variables, and with
#' minimal dependencies. These functions, prefixed with `ll_` rely only on base
#' R, and operate only on objects of type `haven_labelled`. All functions check
#' internally that the variables have the correct class and the correct
#' structure for labelled variables, satisfying the (minimal) specification
#' inherent in the parameter documentation of the [haven::labelled()] function.
#'
#' The constructor, `ll_labelled()`, creates a labelled variable satisfying that
#' specification.
#'
#' @param x A vector to label. Must be either numeric (integer or double) or
#'   character.
#' @param labels A named vector or `NULL`. The vector should be the same type
#'   as `x`. Unlike factors, labels don't need to be exhaustive: only a fraction
#'   of the values might be labelled.
#' @param label A short, human-readable description of the vector.
#'
#' @return A valid labelled variable.
#'
#' @family labelled light
#' @keywords internal
#' @export
ll_labelled <- function(x = double(), labels = NULL, label = NULL) {

  # Construct variable
  x <- as.vector(x)
  class(x) <- c("haven_labelled", "vctrs_vctr", typeof(x))
  attr(x, "labels") <- labels
  attr(x, "label")  <- label

  # Check that the result is valid (an error will be thrown if not)
  ll_assert_labelled(x)

  # Return the result
  x
}

#' Verify that x is a valid labelled variable
#'
#' Verify that x is a valid labelled variable satisfying the (minimal)
#' specification inherent in the parameter documentation of the
#' [haven::labelled()] function for `haven_labelled` objects.
#'
#' @param x A labelled variable
#' @return Invisibly returns x if the check is successful.
#'
#' @family labelled light
#' @keywords internal
#' @export
ll_assert_labelled <- function(x) {

  # Retrieve attributes
  labels <- attr(x, "labels", exact = TRUE)
  label  <- attr(x, "label",  exact = TRUE)

  # Perform the checks
  inherits(x, "haven_labelled")                      ||
    stop('x must be of class "haven_labelled"')
  inherits(x, "vctrs_vctr")                          ||
    stop('x must inherit from "vctrs_vctr"')
  inherits(x, typeof(x))                             ||
    stop('x must inherit from typeof(x)')
  typeof(x) %in% c("double", "integer", "character") ||
    stop('x must be of type "double", "integer", or "character"')
  if (!is.null(labels)) {
    typeof(labels) == typeof(x)                      ||
      stop('labels must be of same type as x')
    !is.null(names(labels))                          ||
      stop("labels must be named")
    !any(duplicated(stats::na.omit(labels)))         ||
      stop("labels must not contain duplicate values")

  }
  if (!is.null(label)) {
    typeof(label) == "character"                     ||
      stop('label must be of type "character"')
    length(label) == 1                               ||
      stop("label must be a single string")
  }

  x
}

#' Get or set variable label of a labelled variable
#'
#' Gets or sets the variable label (`label` attribute) of a labelled vector. The
#' getters/setters should be used rather than manipulating attributes directly,
#' since these functions perform checks to ensure that the result, and the
#' resulting labelled variable, are valid.
#'
#' @param x A labelled variable
#'
#' @family labelled light
#' @keywords internal
#' @rdname ll_var_label
#' @export
ll_var_label <- function(x) {
  ll_assert_labelled(x)
  attr(x, "label",  exact = TRUE)
}

#' @rdname ll_var_label
#' @export
`ll_var_label<-` <- function(x, value) {
  attr(x, "label") <- value
  ll_assert_labelled(x)
}

#' Get or set value labels of a labelled variable
#'
#' Gets or sets the value labels (`labels` attribute) of a labelled vector. The
#' getters/setters should be used rather than manipulating attributes directly,
#' since these functions perform checks to ensure that the result, and the
#' resulting labelled variable, are valid.
#'
#' @param x A labelled variable
#' @param always Always return at least an empty vector of the correct
#'   type, even if the attribute is not set.
#'
#' @family labelled light
#' @keywords internal
#' @rdname ll_val_labels
#' @export
ll_val_labels <- function(x, always = FALSE) {

  # Check args
  ll_assert_labelled(x)
  is.logical(always) || stop('always must be of type "logical"')

  # Prepare and return the result
  labels <- attr(x, "labels",  exact = TRUE)
  if (always && is.null(labels)) {
    # Return an empty, but named, vector
    labels <- vector(typeof(x))
    names(labels) <- character()
  }
  labels
}

#' @rdname ll_val_labels
#' @export
`ll_val_labels<-` <- function(x, value) {
  attr(x, "labels") <- value
  ll_assert_labelled(x)
}

#' Get the character representation of a labelled variable
#'
#' @description
#' Returns a character representation of a labelled variable, using the value
#' labels to look up the label for a given value.
#'
#' The default behavior of this function is similar to
#' [labelled::to_character()]. The options, however, are slightly different.
#' Most importantly, instead of specifying `NA` handling using parameters, the
#' function relies on the `default` parameter to determine what happens for
#' unlabelled variables, allowing users to specify including the original values
#' of `x` instead of the labels, returning `NA`, or returning a specific string
#' value. Also, the default behavior is to drop any variable label attribute, in
#' line with the default [as.character()] method.
#'
#' @param x A labelled variable
#' @param default Vector providing a default label for any values not found in
#'   the `val_labels` (unlabelled values). Must be of length 1 or of the same
#'   length as x. Useful possibilities are `x` (use values where labels are not
#'   found), `NA` (return NA for such values), and `""` (an empty string).
#'   Missing (`NA`) values in `x`, however, are never replaced with the default,
#'   they remain `NA`.
#' @param preserve_var_label Should any `var_label` in x be preserved, or
#'   should they be dropped from the result (ensuring that the result is bare
#'   and without any attributes).
#'
#' @family labelled light
#' @keywords internal
#' @export
ll_to_character <- function(x, default = x, preserve_var_label = FALSE) {

  # Check args
  ll_assert_labelled(x)                # stops if x not valid labelled vector
  is.atomic(default)                   || stop('default must be an atomic vector')
  length(default) %in% c(1, length(x)) || stop('length(default) must be 1 or length(x)')
  is.logical(preserve_var_label)       || stop('preserve_var_label must be of type "logical"')

  # Prepare the result, using match to look up in the labels attribute
  vals <- threadbare(x)
  labs <- ll_val_labels(x, always = TRUE)
  ix <- match(vals, labs)       # Find the values in the labels vector
  result <- names(labs)[ix]  # Return the names for each index

  # Handle default values
  if (length(default) == 1) {
    result[is.na(result)] <- default
  } else {
    result[is.na(result)] <- default[is.na(result)]
  }
  result[is.na(vals)] <- NA     # Missing values in x are always preserved

  # Preserve (copy) variable label if specified
  if (preserve_var_label) {
    attr(result, "label") <- ll_var_label(x)
  }

  # Return the result
  result
}
