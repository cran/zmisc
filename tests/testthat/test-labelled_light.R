
test_that("threadbare() works",{
  # Data
  attach(test_data_labelled_light)

  # Test regular function
  expect_equal(threadbare(zulu_vec), letters)

  # Lists should throw errors
  expect_error(threadbare(cars))
})

test_that("labelled_light ll_labelled() works", {

  # Data
  attach(test_data_labelled_light)

  # Empty constructor
  expect_silent(ll_labelled(letters))

  # The label param
  expect_silent(ll_labelled(letters, label="A labelled variable"))
  expect_error(ll_labelled(letters, label=3))         # Wrong type
  expect_error(ll_labelled(letters, label=zulu_vec))  # Too long

  # The labels param
  expect_silent(ll_labelled(letters, labels = zulu_vec))
  expect_silent(ll_labelled(letters, zulu_vec))
  expect_error(ll_labelled(letters, labels=c(one=1))) # Wrong type
  expect_error(ll_labelled(letters, labels=letters))  # Unnamed
  expect_error(ll_labelled(letters, labels=c(alpha=a, beta=a)))  # Duplicates

})


test_that("labelled_light ll_assert_*() works", {

  # Data
  attach(test_data_labelled_light)

  # Test valid labelled variables
  expect_silent(ll_assert_labelled(fruit_lbl))
  expect_silent(ll_assert_labelled(fruit_lbl_int))
  expect_silent(ll_assert_labelled(fruit_lbl_chr))
  expect_silent(ll_assert_labelled(veggies))
  expect_silent(ll_assert_labelled(exotic_veggies))

  # Test invalid labelled variables
  expect_error(ll_assert_labelled(fruit_fct))
})


test_that("labelled_light ll_var_label() get/set works", {

  # Data
  attach(test_data_labelled_light)

  # Get label
  expect_null(ll_var_label(fruit_lbl))
  expect_match(ll_var_label(veggies), "regular veggies")
  expect_error(ll_var_label(fruit_fct))

  # Set label
  expect_silent(ll_var_label(fruit_lbl) <- "Now with a label")
  expect_match(ll_var_label(fruit_lbl), "Now with a label")
  expect_silent(ll_var_label(fruit_lbl) <- NULL)
  expect_null(ll_var_label(fruit_lbl))
  expect_error(ll_var_label(fruit_lbl) <- 13)

})

test_that("labelled_light ll_val_labels() get/set works", {

  # Data
  attach(test_data_labelled_light)
  letters_lbl <- ll_labelled(letters)
  char_veggie_labels <- c(Carrot = 2, Potato = 3,
    Tomato = 5, Cucumber = 7, Broccoli = 11 )

  # Get label
  expect_null(ll_val_labels(letters_lbl))
  expect_equal(ll_val_labels(veggies), char_veggie_labels)
  expect_error(ll_val_labels(fruit_fct)) # Not labelled variable

  # Set label
  expect_silent(ll_val_labels(fruit_lbl) <- c(OnlyCarrot = 2))
  expect_equal(ll_val_labels(fruit_lbl), c(OnlyCarrot = 2))
  expect_silent(ll_val_labels(fruit_lbl) <- NULL)
  expect_null(ll_val_labels(fruit_lbl))
  expect_silent(ll_val_labels(fruit_lbl)    <- char_veggie_labels)
  expect_error(ll_val_labels(fruit_lbl_chr) <- char_veggie_labels) # Wrong type
  expect_error(ll_val_labels(fruit_lbl_chr) <- letters)            # Unnamed

  # Get from labelled without labels
  expect_null(ll_val_labels(ll_labelled(letters)))
  expect_named(ll_val_labels(ll_labelled(letters), always=TRUE))
})

test_that("labelled_light ll_to_character() works", {

  # Data
  attach(test_data_labelled_light)
  result_fruit_lbl_default <-
    c("Lime", "Peach", "Orange", "Lime", "Apple", "Banana", "Apple",
      "Orange", "Apple", "Apple", "Banana", "Apple", "Peach", "Orange",
      "Banana", "Peach", "Peach", "Orange", "Banana", "Banana", "Lime",
      "Orange", "Peach", "Peach", "Orange", "Banana", "Banana", "Lime",
      "Banana", "Lime", "Apple", "Lime", "Banana", "Apple", "Orange",
      "Lime", "Orange", "Orange", "Lime", "Apple", "Banana", "Banana",
      "Orange", "Peach", "Banana", "Peach", "Lime", "Apple", "Peach",
      "Apple")
  result_veggies_preserve_var_label <-
    structure(c("Tomato", "Broccoli", "Carrot", "Cucumber", "Potato",
      "Tomato", "Cucumber", "Tomato", "Carrot", "Broccoli", "Potato",
      "Carrot", "Cucumber", "Potato", "Tomato", "Cucumber", "Potato",
      "Carrot", "Carrot", "Carrot", "Broccoli", "Broccoli", "Carrot",
      "Carrot", "Potato", "Carrot", "Broccoli", "Tomato", "Cucumber",
      "Potato", "Broccoli", "Carrot", "Broccoli", "Carrot", "Carrot",
      "Cucumber", "Tomato", "Potato", "Potato", "Carrot", "Potato",
      "Broccoli", "Cucumber", "Carrot", "Potato", "Carrot", "Tomato",
      "Carrot", "Cucumber", "Carrot"),
      label = "The regular veggies are indexed by the prime numbers 2-11")
  result_veggies_default <- result_veggies_preserve_var_label
  attributes(result_veggies_default) <- NULL # Drop label and other attr()
  result_veggies_bare <- result_veggies_default
  result_exotic_veggies_fill_na_preserve_var_label <-
    structure(c("Bokchoy", "Okra", "4", "Bokchoy", "Okra", "Okra",
      "Chayote", "16", "Chayote", "Celeriac", "4", "Celeriac", "Taro",
      "Fennel", "Bokchoy", "Celeriac", "4", "Fennel", "Celeriac", "Fennel",
      "Fennel", "Chayote", "Chayote", "Endive", "Bokchoy", "Bokchoy",
      "Bokchoy", "Rutabaga", "Okra", "Jicama", "Endive", "Kohlrabi",
      "Jicama", "Fennel", "Jicama", "Fennel", "Fennel", "8", "16",
      "Endive", "Bokchoy", "16", "Bokchoy", "32", "4", "Chayote", NA,
      "Radicchio", "8", "Chayote"),
      label = paste0("The exotic veggies are indexed by the prime numbers ",
        "2-41. No observations exist for indexes 7 and 41, but 8 unlabelled ",
        "observations exist (for indexes 2, 8, 16 and 32), and 1 observations is NA"))
  result_exotic_veggies_leave_na_preserve_val_label <-
    result_exotic_veggies_fill_na_preserve_var_label
  result_exotic_veggies_leave_na_preserve_val_label[
    result_exotic_veggies_leave_na_preserve_val_label %in% c("2", "4", "8", "16", "32")] <- NA
  result_exotic_veggies_default <- result_exotic_veggies_fill_na_preserve_var_label
  attributes(result_exotic_veggies_default) <- NULL # Drop label and other attr()
  result_exotic_veggies_leave_na <-result_exotic_veggies_leave_na_preserve_val_label
  attributes(result_exotic_veggies_leave_na) <- NULL # Drop label and other attr()
  result_exotic_veggies_na_to_empty_string <- result_exotic_veggies_default
  result_exotic_veggies_na_to_empty_string[
    result_exotic_veggies_na_to_empty_string %in% c("2", "4", "8", "16", "32")] <- ""

  # Test defaults
  expect_equal(ll_to_character(fruit_lbl), result_fruit_lbl_default)
  expect_equal(ll_to_character(veggies), result_veggies_default)
  expect_equal(ll_to_character(exotic_veggies), result_exotic_veggies_default)

  # Test alternative handling
  expect_equal(ll_to_character(exotic_veggies, NA), result_exotic_veggies_leave_na)
  expect_equal(ll_to_character(exotic_veggies, ""), result_exotic_veggies_na_to_empty_string)

  # Test bare
  expect_equal(ll_to_character(veggies, preserve_var_label = FALSE), result_veggies_bare)
  expect_equal(ll_to_character(veggies, preserve_var_label = TRUE), result_veggies_preserve_var_label)

  # labelled vectors without labels should also work
  expect_equal(ll_to_character(ll_labelled(letters[1:3])), letters[1:3])
  expect_equal(ll_to_character(ll_labelled(letters[1:3]), NA), rep(NA_character_, 3))
  expect_equal(ll_to_character(ll_labelled(1:3)), as.character(1:3))
  expect_equal(ll_to_character(ll_labelled(1:3), NA), rep(NA_character_, 3))
  expect_equal(ll_to_character(ll_labelled(c(1.1, 1.2, 1.3))), as.character(c(1.1, 1.2, 1.3)))
  expect_equal(ll_to_character(ll_labelled(c(1.1, 1.2, 1.3)), NA), rep(NA_character_, 3))

})
