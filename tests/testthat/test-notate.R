
# Test suite for the notate function
test_that("notate converts factor and labelled variables as expected", {

  # Helpers to get/set label attribute from unclassed variables
  label <- function(x) {
    attr(x, "label", exact = TRUE)
  }
  "label<-" <- function(x, value) {
     attr(x, "label") <- value
     x
  }

  # Create test data frame
  df <- data.frame(
    var_fct     = factor(c("A", "B", "C")),
    var_fct_na  = factor(c("X", "Y", NA)),
    var_lbl_dbl = ll_labelled(c(1, 2, 3), labels = c("One"=1, "Two"=2, "Three"=3), label = "Labelled doubles"),
    var_lbl_int = ll_labelled(7:9, labels = c("Int7"=7L, "Int8"=8L, "Int9"=9L), label = "Labelled integers"),
    var_lbl_chr = ll_labelled(c("d", "e", "f"), labels = c("delta"="d", "echo"="e", "foxtrot"="f"), label = "Labelled characters"),
    var_lbl_na  = ll_labelled(c(1, 2, NA), labels = c("One"=1)),
    var_lgl = c(TRUE, FALSE, TRUE),
    var_int = 1:3, # Sequences are ints
    var_dbl = 1:3 / 2,
    var_chr = letters[1:3],
    var_cpl = 1:3 * 1+1i,
    var_raw = as.raw(1:3),
    var_dbl_varlabel = 1:3 / 2
  )
  label(df$var_dbl_varlabel) <- "Unlassed with variable label"

  # Test notate function
  noted_df <- notate(df)

  # df |> tibble::as_tibble()
  # noted_df |> tibble::as_tibble()
  # noted_df |> View()

  # Check if notate converts factors correctly
  expect_equal(noted_df$var_fct[1], "[1] A")
  expect_equal(noted_df$var_fct[2], "[2] B")
  expect_equal(noted_df$var_fct[3], "[3] C")

  # Check if notate converts labelled variables correctly
  expect_equal(noted_df$var_lbl_dbl[1], "[1] One")
  expect_equal(noted_df$var_lbl_dbl[2], "[2] Two")
  expect_equal(noted_df$var_lbl_dbl[3], "[3] Three")

  # Check if notate converts labelled variables with missing labels or NAs correctly
  expect_equal(noted_df$var_lbl_na[1], "[1] One")
  expect_equal(noted_df$var_lbl_na[2], "[2]")
  expect_equal(is.na(noted_df$var_lbl_na[3]), TRUE)

  # Check if variable labels are updated correctly
  expect_equal(label(noted_df$var_fct), "<fct>")
  expect_equal(label(noted_df$var_fct_na), "<fct>")
  expect_equal(label(noted_df$var_lbl_dbl), "<lbl> Labelled doubles")
  expect_equal(label(noted_df$var_lbl_int), "<lbl> Labelled integers")
  expect_equal(label(noted_df$var_lbl_chr), "<lbl> Labelled characters")

  # Check if notate.default handles key types correctly
  expect_equal(label(noted_df$var_lgl), "<lgl>")
  expect_equal(label(noted_df$var_int), "<int>")
  expect_equal(label(noted_df$var_dbl), "<dbl>")
  expect_equal(label(noted_df$var_chr), "<chr>")
  expect_equal(label(noted_df$var_cpl), "<cpl>")
  expect_equal(label(noted_df$var_int), "<int>")
  expect_equal(label(noted_df$var_dbl_varlabel), "<dbl> Unlassed with variable label")

})
