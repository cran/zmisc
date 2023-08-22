
test_that("ddply_helper works correctly", {

  # Test with a regular data frame
  df <- data.frame(
    col1 = c(1L, 2L, 1L),
    col2 = c(4, 5, 6)
  )

  result_sum <- ddply_helper(df, sum)
  result_mean <- ddply_helper(df, mean)
  result_sqrt <- ddply_helper(df, sqrt)

  expect_is(result_sum, "data.frame")
  expect_is(result_mean, "data.frame")
  expect_is(result_sqrt, "data.frame")
  expect_error(ddply_helper(df, unique)) # Mismatched column lengths

  # Test with a tibble (if tibble package is available)
  if (requireNamespace("tibble", quietly = TRUE)) {
    tb <- tibble::as_tibble(df)

    result_sum_tb <- ddply_helper(tb, sum)
    result_mean_tb <- ddply_helper(tb, mean)
    result_sqrt_tb <- ddply_helper(tb, sqrt)

    expect_is(result_sum_tb, "tbl_df")
    expect_is(result_mean_tb, "tbl_df")
    expect_is(result_sqrt_tb, "tbl_df")
    expect_error(ddply_helper(tb, unique)) # Mismatched column lengths

    expect_equal(tibble::as_tibble(result_sum), result_sum_tb)
    expect_equal(tibble::as_tibble(result_sqrt), result_sqrt_tb)
  }

  # Test with zero-row data frame
  df_zero_row <- data.frame(x = integer())
  result_zero_sum <- ddply_helper(df_zero_row, sum)
  result_zero_sqrt <- ddply_helper(df_zero_row, sqrt)

  expect_is(result_zero_sum, "data.frame")
  expect_is(result_zero_sqrt, "data.frame")
  expect_equal(ncol(result_zero_sum), 1)  # columns should be maintained
  expect_equal(ncol(result_zero_sqrt), 1) # columns should be maintained
  expect_equal(nrow(result_zero_sum), 1)  # aggregate fun
  expect_equal(nrow(result_zero_sqrt), 0) # regular fun

  # Test with zero-column data frame
  df_zero_col <- data.frame()
  result_zero_col <- ddply_helper(df_zero_col, sum)

  expect_is(result_zero_col, "data.frame")
  expect_equal(ncol(result_zero_col), 0)
  expect_equal(nrow(result_zero_col), 0)

  # Test with malformed input (non-data frame)
  expect_error(ddply_helper(123, sum), "d must be a data.frame")

  # Test with malformed input (non-existing function)
  expect_error(ddply_helper(df, non_existing_fun), "object .* not found")

  # Test applying a non-function
  expect_error(ddply_helper(df, letters), "object .* of mode 'function' was not found")

})
