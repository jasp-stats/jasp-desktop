assertDirExists <- function(x) {
  if (!dir.exists(x))
    stop("Directory ", x, " does not exist", domain = NA)
}

validateJaspDir <- function(dir) {
  assertDirExists(dir)
  expectedDirs <- c("Common", "Desktop", "Engine", "Modules", "R-Interface", "Resources", "Tools")
  if (!all(expectedDirs %in% list.dirs(dir, full.names = FALSE, recursive = FALSE)))
    stop("Invalid jaspDir. Expected these folders: ", paste(expectedDirs, collapse = ", "), domain = NA)
}

validateFlatpakDir <- function(dir) {
  assertDirExists(dir)
  expectedFiles <- c("flathub.json", "org.jaspstats.JASP.json", "RPackages.json")
  if (!all(expectedFiles %in% list.files(dir)))
    stop("Invalid flatpakDir. Expected these files", domain = NA)
}

validateGithubPath <- function() {
  if (Sys.getenv("GITHUB_PAT") == "")
    stop("GITHUB_PAT is not set!", domain = NA)
}

validateSetup <- function(jaspDir, flatpakDir) {
  # call this before downloading pkgs
  validateJaspDir(jaspDir)
  validateFlatpakDir(flatpakDir)
  validateGithubPath()
}

validateGithubPkgs <- function(dirs) {

  errorMessages <- character()
  allFiles <- list.files(dirs["local-github"])

  r <- "^(.*)_contents_DESCRIPTION\\?ref=(.+)$"
  matches <- regmatches(allFiles, regexec(r, allFiles))
  matches <- matches[lengths(matches) > 0L]

  SHAs <- unlist(lapply(matches, `[[`, 3L), use.names = FALSE)
  names(SHAs) <- unlist(lapply(matches, `[[`, 2L), use.names = FALSE)

  # check 1: verify that all the index files match the names - this check is no good if people explicitly specify a commit in the DESCRIPTION
  # indexFiles <- allFiles[grep("^([^_]*_){1}[^_]*$", allFiles)]
  # diff <- setdiff(names(SHAs), indexFiles)
  # if (length(diff) > 0L)
  #   errorMessages <- c(errorMessages, paste0("These _contents_DESCRIPTION are missing an index file: ", paste0(diff, collapse = ",")))

  # check 2: verify that every "*_contents_description*" has an associated tarball
  tarballs <- file.path(dirs["local-github"], paste0(names(SHAs), "_tarball_", SHAs))
  missing <- which(!file.exists(tarballs))
  if (length(missing) > 0L)
    errorMessages <- c(errorMessages, paste0("These github packages are missing a tarball: ", paste(names(SHAs)[missing], collapse = ", ")))

  if (length(errorMessages) > 0L)
    stop("These error message occurred:\n\n", paste(errorMessages, collapse = "\n"))

  return(invisible(TRUE))

}

validateV8folder <- function(dirs) {

  v8Dir <- file.path(dirs["other-dependencies"], "v8")
  if (!dir.exists(v8Dir))
    stop("V8 dir does not exist at ", v8Dir, domain = NA)

  subdirs <- file.path(v8Dir, c("include", "lib", "lic"))
  if (!all(dir.exists(subdirs)))
    stop("V8 dir does not contain the following subdirectories: ", paste(subdirs, collapse = ", "), domain = NA)


}

validateFlatpakFolder <- function(dirs) {
  # call this before creating one tar.gz
  validateGithubPkgs(dirs)
  validateV8folder(dirs)
}
