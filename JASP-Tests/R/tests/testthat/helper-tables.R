expect_equal_tables <- function(test, ref, ...) {
  test <- jasptools:::collapseTable(test)
  expect_equal(test, ref, tolerance=1e-4, ...)
}
