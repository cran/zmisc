
#' Sample from a vector in a safe way
#'
#' @description
#' The [zample()] function duplicates the functionality of [sample()], with the
#' exception that it does not attempt the (sometimes dangerous)
#' user-friendliness of switching the interpretation of the first element to a
#' number if the length of the vector is 1. `zample()` *always* treats its first
#' argument as a vector containing elements that should be sampled, so your code
#' won't break in unexpected ways when the input vector happens to be of length
#' 1.
#'
#' @details
#' If what you really want is to sample from an interval between 1 and n, you can
#' use `sample(n)` or `sample.int(n)` (but make sure to only pass vectors of
#' length one to those functions).
#'
#' @param x       The vector to sample from
#' @param size    The number of elements to sample from `x` (defaults to `length(x)`)
#' @param replace Should elements be replaced after sampling (defaults to `false`)
#' @param prob    A vector of probability weights (defaults to equal probabilities)
#'
#' @return The resulting sample
#'
#' @examples
#' # For vectors of length 2 or more, zample() and sample() are identical
#' set.seed(42); zample(7:11)
#' set.seed(42); sample(7:11)
#'
#' # For vectors of length 1, zample() will still sample from the vector,
#' # whereas sample() will "magically" switch to interpreting the input
#' # as a number n, and sampling from the vector 1:n.
#' set.seed(42); zample(7)
#' set.seed(42); sample(7)
#'
#' # The other arguments work in the same way as for sample()
#' set.seed(42); zample(7:11, size=13, replace=TRUE, prob=(5:1)^3)
#' set.seed(42); sample(7:11, size=13, replace=TRUE, prob=(5:1)^3)
#'
#' # Of course, sampling more than the available elements without
#' # setting replace=TRUE will result in an error
#' set.seed(42); tryCatch(zample(7, size=2), error=wrap_error)
#'
#' @export
zample = function (x, size=length(x), replace = FALSE, prob = NULL)
{
    # Bail out of sampling from data.frames, use dplyr::sample_X() for that
    if (inherits(x, "data.frame"))
    {
        stop("zulutils::zample() does not support data.frames")
    }

    # Sampling zero elements from a zero length vector should be allowed
    if ( size==0 & length(x)==0 ) { return(vector(mode=mode(x))) }

    # The code from original sample(), minus the silly numeric handling
    x[sample.int(length(x), size, replace, prob)]
}

