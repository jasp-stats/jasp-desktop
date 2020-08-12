args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0)
  stop("\nRequired arguments:\n",
       "\t(1) Path to jasp-desktop folder.\n",
       "\t(2) Optionally, boolean whether to install packages.\n",
       "\t(3) Optionally, boolean if a list of all required packages and their dependencies should be printed.")

jaspDir <- args[1]

install <- FALSE
if (length(args) > 1)
  install <- ifelse(tolower(args[2]) == "true", TRUE, FALSE)

printList <- FALSE
if (length(args) > 2)
  printList <- ifelse(tolower(args[3]) == "true", TRUE, FALSE)

if (!dir.exists(jaspDir))
  stop("Could not find directory ", jaspDir)

subDirs <- list.dirs(jaspDir, recursive=FALSE, full.names=FALSE)
if (!all(c("JASP-Engine", "JASP-R-Interface") %in% subDirs))
  stop("Could not locate JASP-Engine and JASP-R-Interface as subdirectories of ", jaspDir)

dirs <- c(file.path(jaspDir, "JASP-Engine"), file.path(jaspDir, "JASP-R-Interface"))
rFiles <- list.files(dirs, pattern="\\.[Rr]$", recursive=TRUE, full.names=TRUE)
if (length(rFiles) == 0)
  stop("Could not locate any R files in the JASP-Engine and JASP-R-Interface directories")

options("repos" = "https://cloud.r-project.org")
if (!"stringr" %in% installed.packages())
  install.packages("stringr") # needed to generate the required packages list more easily

reqPkgs <- NULL
expr <- '([a-zA-Z0-9.]{2,}(?<![.]))(?:::|:::)[a-zA-Z0-9._]+' # valid namespace -> triple or double colons -> valid function name
comment <- '#.*'
for (rFile in rFiles) {
  content <- suppressWarnings(readLines(rFile))
  content <- gsub(comment, "", content) # remove comments
  matches <- stringr::str_match_all(content, expr)
  matches <- unlist(lapply(matches, function(match) match[, 2]))
  if (length(matches) == 0)
    next
  reqPkgs <- c(reqPkgs, matches)
}

# some packages are not detected correctly as a dependency (e.g., GPArotation is incorrectly marked as "Suggest' in psych)
reqPkgs <- c(reqPkgs, "GPArotation","RcppArmadillo", "modules", "remotes", "Rcsdp")

# exclude all packages shipped by JASP
reqPkgs <- reqPkgs[!reqPkgs %in% c('jaspGraphs', "jaspResults", "jaspTools")]

# remove the duplicates
reqPkgs <- sort(unique(reqPkgs))

# remove the base packages
basePkgs <- installed.packages(priority="high")
basePkgs <- basePkgs[basePkgs[, "Priority"] == "base", 1]
reqPkgs <- reqPkgs[!reqPkgs %in% basePkgs]

if (install) {
  pkgsToinstall <- reqPkgs[!reqPkgs %in% installed.packages()]
  if (length(pkgsToinstall) > 0) {
    cat("Installing all missing packages...")
    for (pkg in pkgsToinstall)
      install.packages(pkg, dependencies = c("Depends", "Imports"), INSTALL_opts = c("--no-docs", "--no-html", "--no-multiarch"))
    cat("\nFinished iterating over the required packages\n")
  }
} else {
  cat("\nInstall string:\n")
  strPkgs <- paste0("'", reqPkgs, "'")
  installString <- paste0("install.packages(c(", paste(strPkgs, collapse=", "), "), repos = 'https://cloud.r-project.org', dependencies = c('Depends', 'Imports'))")
  cat(installString)
}

if (printList) {
  deps <- tools::package_dependencies(reqPkgs, recursive=TRUE, which=c('Depends', 'Imports'))
  depPkgs <- unlist(deps)
  depPkgs <- sort(unique(depPkgs))
  depPkgs <- depPkgs[!depPkgs %in% reqPkgs]
  cat("\n\nRequired packages:\n")
  cat(paste0(reqPkgs, collapse=", "), "\n")
  cat("\nDependencies of required packages [Imports, Depends]:\n")
  cat(paste0(depPkgs, collapse=", "), "\n")
}
