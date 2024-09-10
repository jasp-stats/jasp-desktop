args <- commandArgs()
#print(args)

pkgName <- args[[ length(args)     ]]
pkgLib  <- args[[ length(args) - 1 ]]
#print(pkgName)
#print(pkgLib)
# usage

# R --file=renv_new_idea_find_package_path.R --args library packagename [subdir 1] [subdir 2] ...

# throws if the package or directory does not exist.


# R-4.3.1 --file=renv_new_idea_find_package_path.R --args /home/don/R/x86_64-pc-linux-gnu-library/4.2 Rcpp
# R-4.3.1 --file=renv_new_idea_find_package_path.R --args /home/don/R/x86_64-pc-linux-gnu-library/4.2 Rcpp include
# args <- c(
#   "/home/don/R/x86_64-pc-linux-gnu-library/4.2",
#   "Rcpp"
# )
result <- find.package(package = pkgName, lib.loc = pkgLib)

#if (length(args) > 2L)
#  result <- do.call(file.path, c(result, as.list(args[3:length(args)])))

if (!(dir.exists(result) || file.exists(result)))
  stop("target file/ directory does not exist!")

cat(normalizePath(result))
