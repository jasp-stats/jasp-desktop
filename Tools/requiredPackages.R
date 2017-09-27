args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("\nRequires: path to R folder")
} else if (length(args) == 1) {
  lib <- args[1]
} else {
  stop("\nAccepts only one argument")
}

if (dir.exists(lib)) {
  if (any(endsWith(list.files(lib), ".R"))) {
    setwd(lib)
  } else {
    stop("Could not find any R files in the directory")
  }
} else {
  stop("Could not find directory")
}

options("repos" = "https://cloud.r-project.org")

if (! "stringr" %in% installed.packages()) {
  install.packages("stringr")
}
library(stringr)

basePkgs <- installed.packages(priority="high")
basePkgs <- basePkgs[basePkgs[, "Priority"] == "base", 1]

reqPkgs <- NULL
expr <- '([a-zA-Z0-9.]{2,}(?<![.]))(?:::|:::)[a-zA-Z0-9._]+'
# matches namespace in [valid namespace -> triple or double colons -> valid function name]
comment <- '#.*'
files <- list.files(getwd(), pattern="\\.[Rr]$")
for (file in files) {
  content <- suppressWarnings(readLines(file))
  content <- gsub(comment, "", content) # remove comments
  found <- stringr::str_match(content, expr)
  matches <- found[! is.na(found)[, 1], , drop=FALSE]
  if (nrow(matches) == 0) {
    next
  }
  reqPkgs <- c(reqPkgs, matches[, 2])
}

#Temporarly add the GPArotation manually (incorrectdly marked as "Suggest' in pshych) 
reqPkgs <- c(reqPkgs, "GPArotation")
reqPkgs <- sort(unique(reqPkgs))
reqPkgs <- reqPkgs[! reqPkgs %in% basePkgs]

strPkgs <- paste0("'", reqPkgs, "'")
installString <- paste0("install.packages(c(", paste(strPkgs, collapse=", "), "), repos = 'https://cloud.r-project.org', dependencies = NA)")


deps <- tools::package_dependencies(reqPkgs, recursive=TRUE)
depPkgs <- unlist(deps)
depPkgs <- sort(unique(depPkgs))

allPkgs <- sort(unique(c(reqPkgs, depPkgs)))

cat("\nInstall string:\n")
cat(installString)
cat("\n\nRequired packages:\n")
cat(paste0(reqPkgs, collapse="\n"), "\n")
cat("\nDependencies of required packages [Imports, Depends, LinkingTo]:\n")
cat(paste0(depPkgs, collapse="\n"), "\n")
cat("\nFull list of packages:\n")
cat(paste0(allPkgs, collapse="\n"), "\n")
