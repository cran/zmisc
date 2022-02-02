zmisc
================

<!-- README.md is generated from README.Rmd -->
<!-- badges: start -->
<!-- badges: end -->

## Vector Look-Ups and Safer Sampling

A collection of utility functions that facilitate looking up vector
values from a lookup table, and support a safer approach to vector
sampling, sequence generation, and aggregation.

## Installation

You can install the released version of `zmisc` from
[CRAN](https://cran.r-project.org/package=zmisc) with:

``` r
install.packages("zmisc")
```

You can install the development version of `zmisc` from
[GitHub](https://github.com/torfason/zmisc) with:

``` r
remotes::install_github("torfason/zmisc")
```

## Usage

In order to use the package, you generally want to attach it first:

``` r
library(zmisc)
```

## Quick and easy value lookups

The functions
[lookup()](https://torfason.github.io/zmisc/reference/lookup.html) and
[lookuper()](https://torfason.github.io/zmisc/reference/lookuper.html)
are used to look up values from a lookup table, which can be supplied as
a `vector`, a `list`, or a `data.frame`. The functions are in some ways
similar to the Excel function `VLOOKUP()`, but are designed to work
smoothly in an R workflow, in particular within pipes.

### lookup: Lookup values from a lookup table

The [lookup()](https://torfason.github.io/zmisc/reference/lookup.html)
function implements lookup of certain strings (such as variable names)
from an lookup table which maps keys onto values (such as variable
labels or descriptions). Original values are returned if they are not
found in the lookup table.

The lookup table can be in the form of a two-column `data.frame`, in the
form of a named `vector`, or in the form of a `list`. If the table is in
the form of a `data.frame`, the lookup columns should be named `name`
(for the key) and `value` (for the value). If the lookup table is in the
form of a named `vector` or `list`, the name is used for the key, and
the returned value is taken from the values in the vector or list.

Any names in x are not included in the result.

#### Examples

``` r
fruit_lookup_vector <- c(a="Apple", b="Banana", c="Cherry")
lookup(letters[1:5], fruit_lookup_vector)

mtcars_lookup_data_frame <- data.frame(
  name = c("mpg", "hp", "wt"),
  value = c("Miles/(US) gallon", "Gross horsepower", "Weight (1000 lbs)"))
lookup(names(mtcars), mtcars_lookup_data_frame)
```

### lookuper: Construct lookup function based on a specific lookup table

The
[lookuper()](https://torfason.github.io/zmisc/reference/lookuper.html)
function returns *a function* equivalent to the
[lookup()](https://torfason.github.io/zmisc/reference/lookup.html)
function, except that instead of taking a lookup table as an argument,
the lookup table is embedded in the function itself.

This can be very useful, in particular when using the lookup function as
an argument to other functions that expect a function which maps
`character`->`character` but do not offer a good way to pass additional
arguments to that function.

#### Examples

``` r
lookup_fruits <- lookuper(list(a="Apple", b="Banana", c="Cherry"))
lookup_fruits(letters[1:5])
```

## Safer sampling, sequencing and aggregation

The functions
[zample()](https://torfason.github.io/zmisc/reference/zample.html),
[zeq()](https://torfason.github.io/zmisc/reference/zeq.html), and
[zingle()](https://torfason.github.io/zmisc/reference/zingle.html) are
intended to make your code less likely to break in mysterious ways when
you encounter unexpected boundary conditions. The
[zample()](https://torfason.github.io/zmisc/reference/zample.html) and
[zeq()](https://torfason.github.io/zmisc/reference/zeq.html) are almost
identical to the
[sample()](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sample)
and
[seq()](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/seq)
functions, but a bit safer.

### zample: Sample from a vector in a safe way

The [zample()](https://torfason.github.io/zmisc/reference/zample.html)
function duplicates the functionality of
[sample()](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sample),
with the exception that it does not attempt the (sometimes dangerous)
user-friendliness of switching the interpretation of the first element
to a number if the length of the vector is 1. `zample()` *always* treats
its first argument as a vector containing elements that should be
sampled, so your code won’t break in unexpected ways when the input
vector happens to be of length 1.

#### Examples

``` r
# For vectors of length 2 or more, zample() and sample() are identical
set.seed(42); zample(7:11)
set.seed(42); sample(7:11)

# For vectors of length 1, zample() will still sample from the vector,
# whereas sample() will "magically" switch to interpreting the input
# as a number n, and sampling from the vector 1:n.
set.seed(42); zample(7)
set.seed(42); sample(7)

# The other arguments work in the same way as for sample()
set.seed(42); zample(7:11, size=13, replace=TRUE, prob=(5:1)^3)
set.seed(42); sample(7:11, size=13, replace=TRUE, prob=(5:1)^3)

# Of course, sampling more than the available elements without
# setting replace=TRUE will result in an error
set.seed(42); tryCatch(zample(7, size=2), error=wrap_error)
```

### zeq: Generate sequence in a safe way

The [zeq()](https://torfason.github.io/zmisc/reference/zeq.html)
function creates an increasing integer sequence, but differs from the
standard one in that it will not silently generate a decreasing sequence
when the second argument is smaller than the first. If the second
argument is one smaller than the first it will generate an empty
sequence, if the difference is greater, the function will throw an
error.

#### Examples

``` r
# For increasing sequences, zeq() and seq() are identical
zeq(11,15)
zeq(11,11)

# If second argument equals first-1, an empty sequence is returned
zeq(11,10)

# If second argument is less than first-1, the function throws an error
tryCatch(zeq(11,9), error=wrap_error)
```

### zingle: Return the single (unique) value found in a vector

The [zingle()](https://torfason.github.io/zmisc/reference/zingle.html)
function returns the first element in a vector, but only if all the
other elements are identical to the first one (the vector only has a
`zingle` value). If the elements are not all identical, it throws an
error. The vector must contain at least one non-`NA` value, or the
function errors out as well. This is especially useful in aggregations,
when all values in a given group should be identical, but you want to
make sure.

#### Examples

``` r
# If all elements are identical, all is good.
# The value of the element is returned.
zingle(c("Alpha", "Alpha", "Alpha"))

# If any elements differ, an error is thrown
tryCatch(zingle(c("Alpha", "Beta", "Alpha")), error=wrap_error)

if (require("dplyr", quietly=TRUE, warn.conflicts=FALSE)) {
  d <- tibble::tribble(
    ~id, ~name, ~fouls,
    1, "James", 3,
    2, "Jack",  2,
    1, "James", 4
  )

  # If the data is of the correct format, all is good
  d %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(name=zingle(name), total_fouls=sum(fouls))
 }

if (require("dplyr", quietly=TRUE, warn.conflicts=FALSE)) {
  # If a name does not match its ID, we should get an error
  d[1,"name"] <- "Jammes"
  tryCatch({
    d %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(name=zingle(name), total_fouls=sum(fouls))
  }, error=wrap_error)
}
```
