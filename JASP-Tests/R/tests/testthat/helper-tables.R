expect_equal_tables <- function(test, ref, ...) {
  test <- JASPTools:::collapseTable(test)
  expect_equal(test, ref, tolerance=1e-5, ...)
}
