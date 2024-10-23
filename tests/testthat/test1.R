

test_that("OR_95CI returns correctly formatted result", {

  coef <- 0.5
  se <- 0.2
  siglevel <- 0.05
  roundto <- 2

  result <- OR_95CI(coef, se, siglevel, roundto)
  OR_val <- exp(coef)  # Expected OR value

  expect_equal(OR_val, 1.65, tolerance = 1e-2)  # 1.65 with small tolerance
  print(result)  # Temporary, remove after debugging

  ## correctly formatted CI
  expect_true(grepl("1\\.65 \\(1\\.11, 2\\.44\\)", result))  # Test the full string format

}
)
