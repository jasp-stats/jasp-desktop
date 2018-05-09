print(getwd())
print(dir())
print(.libPaths())

lib <- .libPaths()[1]
repos <- "https://cloud.r-project.org"

script <- readLines("JASP-Engine/JASP/R/packagecheck.R")

lst <- regmatches(script, gregexpr("(?<=\')(.*?)(?=\')", script, perl = TRUE))
lst <- lst[lengths(lst) > 0]
pkgs <- do.call(rbind, lst)[, c(1, 3)]

installed <- installed.packages(priority = "NA")

for (i in 1:nrow(pkgs)) {
    idx <- which(pkgs[i, 1] %in% installed[, "Package"])
    if (length(idx) == 0 || pkgs[, 2] != installed[idx, "Version"]) {
	devtools::install_version(package = pkgs[i, 1], version = pkgs[i, 2], lib = lib,
							  upgrade_dependencies = FALSE, build_vignettes = FALSE,
							  keep_source = FALSE, repos = repos)
    }
}
