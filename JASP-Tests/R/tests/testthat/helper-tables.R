expect_equal_tables <- function(test, ref, ...) {
  test <- jasptools:::collapseTable(test)
  ref <- jasptools:::collapseTable(ref)
  expect_equal(test, ref, tolerance=1e-5, ...)
}
