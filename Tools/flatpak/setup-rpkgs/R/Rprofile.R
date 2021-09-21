
.lib64 <- if (dir.exists("/app")) {
  file.path("file:", "app", "lib64")
  cat("/app exists!\n")
  "/app/lib64"
} else {
  file.path("file:", normalizePath("flatpak_folder/flatpak-helper/local-github"))
  cat("/app does not exist!\n")
  "app/lib64"
}

stripUrl <- function(url) {
  gsub("/+", "_", strsplit(url, "repos/", fixed = TRUE)[[1L]][2])
}

download_override_flatpak <- function(url, destfile, mode = "wb", quiet = FALSE, headers = NULL) {

  options("renv.download.override" = NULL)
  on.exit(options("renv.download.override" = download_override_flatpak))

  type <- get("type", parent.frame(1))
  if (identical(type, "github")) {
    cat("old url:", url, '\n')
    url <- file.path(.lib64, "local-github", stripUrl(url))
    cat("new url:", url, '\n')
  }

  renv:::download(url, destfile, type = type, quiet = quiet, headers = headers)

}

# this will only take effect AFTER flatpakRegenerateRenvStructure.R was run
if (dir.exists(file.path(.lib64, "local-github"))) {
  cat("overriding renv download function\n")
  options("renv.download.override" = download_override_flatpak)
} else {
  cat("not overriding renv download function\n")
}
