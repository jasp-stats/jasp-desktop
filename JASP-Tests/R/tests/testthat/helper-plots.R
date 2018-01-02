expect_equal_plots <- function(test, name, dir) {
  vdiffr::expect_doppelganger(paste(dir, name, sep="-"), test, path=dir)
}
