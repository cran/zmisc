
#' Embed factor levels and value labels in values.
#'
#' @description
#' This function adds level/label information as an annotation to either factors
#' or `labelled` variables. This function is called `notate()` rather than
#' `annotate()` to avoid conflict with `ggplot2::annotate()`. It is a generic that
#' can operate either on individual vectors or on a `data.frame`.
#'
#' When printing `labelled` variables from a `tibble` in a console, both the
#' numeric value and the text label are shown, but no variable labels. When
#' using the `View()` function, only variable labels are shown but no value
#' labels. For factors, there is no way to view the integer levels and values at
#' the same time.
#'
#' In order to allow the viewing of both variable and value labels at the same
#' time, this function converts both `factor` and `labelled` variables to
#' `character`, including both numeric levels (`labelled` values) and character
#' values (`labelled` labels) in the output.
#'
#' @param x The object (either vector or `date.frame` of vectors), that one
#'   desires to annotate and/or view.
#'
#' @return The processed `data.frame`, suitable for viewing, in particular
#'   through the `View()` function.
#'
#' @examples
#' d <- data.frame(
#'   chr = letters[1:4],
#'   fct = factor(c("alpha", "bravo", "chrly", "delta")),
#'   lbl = ll_labelled(c(1, 2, 3, NA),
#'                     labels = c(one=1, two=2),
#'                     label = "A labelled vector")
#' )
#' dn <- notate(d)
#' dn
#' # View(dn)
#'
#' @export
notate <- function(x) {
  UseMethod("notate")
}

#' @export
notate.default <- function(x) {
  type_short <- typeof(x) |> lookup_types_short()
  attr(x, "label") <- paste_na(
    paste0("<", type_short, ">"),
    attr(x, "label"))
  x
}

#' @export
notate.data.frame <- function(x) {
  # Apply to individual columns
  ddply_helper(x, notate)
}

#' @export
notate.factor <- function(x) {
  is.factor(x) || stop("x must be a factor")
  r <- rep(c(character(0), NA), length(x))
  r[!is.na(x)] <- paste0("[", as.numeric(x[!is.na(x)]), "] ", as.character(x[!is.na(x)]))
  attr(r, "label") <- paste_na("<fct>", attr(x, "label")) # (ll_var_label() requires correct class)
  r
}

#' @export
notate.haven_labelled <- function(x) {
  ll_assert_labelled(x)
  vals   <- as.vector(x)
  labs_n <- ll_to_character(x, default = NA)
  r <- rep(c(character(0), NA), length(x))
  r[!is.na(x)] <- paste_na(
    paste0("[", vals[!is.na(x)], "]"),
    labs_n[!is.na(x)])
  attr(r, "label") <- paste_na("<lbl>", ll_var_label(x)) # (ll_var_label() requires correct class)
  r
}


# Helper to look up the short types given typeof()
lookup_types_short <- lookuper(
  c(logical = "lgl",  integer = "int", double = "dbl",
    character = "chr", complex = "cpl"))

# Helper to suppress NAs in paste
# https://stackoverflow.com/questions/13673894/suppress-nas-in-paste
paste_na <- function(..., sep = " ") {
  values <- cbind(...)
  apply(values, 1, function(x) paste(x[!is.na(x)], collapse = sep))
}
# paste_na(c(1,1), c(2, NA))
# paste_na(c(1,1), c(2, NA), 4:5)
