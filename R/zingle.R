
#' Return the single (unique) value found in a vector
#'
#' @description
#' The [zingle()] function returns the first element in a vector, but only if
#' all the other elements are identical to the first one (the vector only has a
#' `zingle` value). If the elements are not all identical, it throws an error.
#' The vector must contain at least one non-`NA` value, or the function errors
#' out as well. This is especially useful in aggregations, when all values in a
#' given group should be identical, but you want to make sure.
#'
#' @details
#' Optionally takes a `na.rm` parameter, similarly to sum, mean and other
#' aggregate functions. If `TRUE`, `NA` values will be removed prior to
#' comparing the elements, so the function will accept input values that contain
#' a combination of the single value and any `NA` values (but at least one
#' non-`NA` value is required).
#'
#' Only values are tested for equality. Any names are simply ignored, and the
#' result is an unnamed value. This is in line with how other aggregation
#' functions handle names.
#'
#' @param x Vector of elements that should all be identical
#' @param na.rm Should `NA` elements be removed prior to comparison
#' @return The `zingle` element in the vector
#'
#' @examples
#' # If all elements are identical, all is good.
#' # The value of the element is returned.
#' zingle(c("Alpha", "Alpha", "Alpha"))
#'
#' # If any elements differ, an error is thrown
#' tryCatch(zingle(c("Alpha", "Beta", "Alpha")), error=wrap_error)
#'
#' if (require("dplyr", quietly=TRUE, warn.conflicts=FALSE)) {
#'   d <- tibble::tribble(
#'     ~id, ~name, ~fouls,
#'     1, "James", 3,
#'     2, "Jack",  2,
#'     1, "James", 4
#'   )
#'
#'   # If the data is of the correct format, all is good
#'   d %>%
#'     dplyr::group_by(id) %>%
#'     dplyr::summarise(name=zingle(name), total_fouls=sum(fouls))
#'  }
#'
#' if (require("dplyr", quietly=TRUE, warn.conflicts=FALSE)) {
#'   # If a name does not match its ID, we should get an error
#'   d[1,"name"] <- "Jammes"
#'   tryCatch({
#'     d %>%
#'       dplyr::group_by(id) %>%
#'       dplyr::summarise(name=zingle(name), total_fouls=sum(fouls))
#'   }, error=wrap_error)
#' }
#
#' @export
zingle = function(x, na.rm = FALSE)
{
  if (na.rm) x = x[!is.na(x)]
  stopifnot(all(x[1]==x))
  unname(x[1])
}

