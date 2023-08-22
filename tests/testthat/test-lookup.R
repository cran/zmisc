
#### test lookup() ####
test_that("lookup works", {

  # Extremely simple test with letters
  expect_equal(lookup(letters, c(a="A",b="B")), c("A","B",letters[3:26]))

  # Validation data
  d.pets <- data.frame(
    name  = c("cat",    "lizard",  "parrot"),
    value = c("mammal", "reptile", "bird")
  )
  x.species  = c("lizard", "cat")
  x.kingdoms = c("reptile","mammal")

  # Standard lookup
  expect_equal( lookup(x.species,d.pets),  x.kingdoms  )

  # Order should not matter, only the names of the name/value columns.
  d.pets.rearranged = d.pets %>%
    dplyr::mutate(ga = "ga", rb="rb", age="age") %>%
    dplyr::select(ga,name,rb,value,age)
  expect_equal( lookup(x.species,d.pets.rearranged),  x.kingdoms)
})


#### test lookuper() ####
test_that("lookuper works", {

  # Extremely simple test with letters
  lookup_letters <- lookuper(c(a = "A", b = "B"))
  expect_equal(lookup_letters(letters), c("A", "B", letters[3:26]))

  # Validation data
  d.pets <- data.frame(
    name  = c("cat",    "lizard",  "parrot"),
    value = c("mammal", "reptile", "bird")
  )
  x.species <- c("lizard", "cat")
  x.kingdoms <- c("reptile", "mammal")

  # Standard lookup
  lookup_pets <- lookuper(d.pets)
  expect_equal(lookup_pets(x.species), x.kingdoms)

  # Order should not matter, only the names of the name/value columns.
  d.pets.rearranged <- d.pets %>%
    dplyr::mutate(ga = "ga", rb = "rb", age = "age") %>%
    dplyr::select(ga, name, rb, value, age)
  lookup_pets_rearranged <- lookuper(d.pets.rearranged)
  expect_equal(lookup_pets_rearranged(x.species), x.kingdoms)
})


#### test standardize_lookup_table() ####
test_that("standardize_lookup_table works", {

  # house cat, because spaces should not pose a problem
  d.in <- data.frame(
    name  = c("house cat", "lizard",  "parrot"),
    value = c("mammal",    "reptile", "bird")
  ) #|> standardize_lookup_table() |> dput()


  l.want <- list("house cat" = "mammal", lizard = "reptile", parrot = "bird")
  l.have <- standardize_lookup_table(d.in)

  expect_equal(l.have, l.want)

})

#### test missing values####
test_that("looking up missing values works", {
  d.lookup <- data.frame(
    name  = c("house cat", "lizard",  "parrot"),
    value = c("mammal",    "reptile", "bird")
  )

  have.default = lookup(c("lizard", "parrot", "house cat", "tiger", "", NA), d.lookup)
  want.default = c("reptile", "bird", "mammal", "tiger", "", NA)
  expect_equal(have.default, want.default)

  have.na = lookup(c("lizard", "parrot", "house cat", "tiger", "", NA), d.lookup, default = NA)
  want.na = c("reptile", "bird", "mammal", NA, NA, NA)
  expect_equal(have.default, want.default)

  # Note that NA values are never found in the lookup table, and so are replaced with the default
  have.empty = lookup(c("lizard", "parrot", "house cat", "tiger", "", NA), d.lookup, default = "")
  want.empty = c("reptile", "bird", "mammal", "", "", "")
  expect_equal(have.empty, want.empty)

  lookup_animals_default <- lookuper(d.lookup)
  have.lookuper.default <- lookup_animals_default(c("lizard", "parrot", "house cat", "tiger", "", NA))
  expect_equal(have.lookuper.default, want.default)

  lookup_animals_na <- lookuper(d.lookup, default = NA)
  have.lookuper.na <- lookup_animals_na(c("lizard", "parrot", "house cat", "tiger", "", NA))
  expect_equal(have.lookuper.na, want.na)
})
