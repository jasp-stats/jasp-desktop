args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("\nRequires (1) path to R folder and optionally (2) boolean whether to install packages")
} else if (length(args) == 1) {
  lib <- args[1]
  install <- FALSE
} else if (length(args) == 2) {
  lib <- args[1]
  install <- ifelse(tolower(args[2]) == "true", TRUE, FALSE)
} else {
  stop("\nAccepts only two arguments")
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
# Expression to match namespace in [valid namespace -> triple or double colons -> valid function name]:
expr <- '([a-zA-Z0-9.]{2,}(?<![.]))(?:::|:::)[a-zA-Z0-9._]+'
comment <- '#.*'
files <- list.files(getwd(), pattern="\\.[Rr]$")
for (file in files) {
  content <- suppressWarnings(readLines(file))
  content <- gsub(comment, "", content) # remove comments
  matches <- stringr::str_match_all(content, expr)
  matches <- unlist(lapply(matches, function(match) match[, 2]))
  if (length(matches) == 0)
    next
  reqPkgs <- c(reqPkgs, matches)
}

# Temporarly add the GPArotation manually (incorrectly marked as "Suggest' in psych) 
reqPkgs <- c(reqPkgs, "GPArotation")
reqPkgs <- reqPkgs[!reqPkgs %in% 'JASPgraphs']
# Exclude jasptools manually (should not be shipped)
basePkgs <- c(basePkgs, "jasptools")
reqPkgs <- sort(unique(reqPkgs))
reqPkgs <- reqPkgs[! reqPkgs %in% basePkgs]

if (install) {
  cat("Installing all missing packages...")
  for (pkg in reqPkgs) {
    if (! pkg %in% installed.packages()) {
      install.packages(pkg, repos = 'https://cloud.r-project.org', dependencies = c("Depends", "Imports"))
    }
  }
  cat("\nFinished iterating over the required packages\n")
} else {
  strPkgs <- paste0("'", reqPkgs, "'")
  installString <- paste0("install.packages(c(", paste(strPkgs, collapse=", "), "), repos = 'https://cloud.r-project.org', dependencies = c('Depends', 'Imports'))")

  deps <- tools::package_dependencies(reqPkgs, recursive=TRUE, which=c('Depends', 'Imports'))
  depPkgs <- unlist(deps)
  depPkgs <- sort(unique(depPkgs))

  allPkgs <- sort(unique(c(reqPkgs, depPkgs)))

  cat("\nInstall string:\n")
  cat(installString)
  cat("\n\nRequired packages:\n")
  cat(paste0(reqPkgs, collapse="\n"), "\n")
  cat("\nDependencies of required packages [Imports, Depends]:\n")
  cat(paste0(depPkgs, collapse="\n"), "\n")
  cat("\nFull list of packages:\n")
  cat(paste0(allPkgs, collapse="\n"), "\n")
}
