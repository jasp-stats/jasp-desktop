
# Dont go checking github all the time and triggering rate-limiting, for https://github.com/jasp-stats/INTERNAL-jasp/issues/1166
remote_sha.github_remote2 <- function (remote, ..., use_curl = !remotes:::is_standalone() && remotes:::pkg_installed("curl")) {
  if (remotes:::pkg_installed(remote$repo))
    return(NA_character_)

  `%||%` <- remotes:::`%||%`

  tryCatch(
    remotes:::github_commit(
      username = remote$username,
      repo     = remote$repo,
      host     = remote$host,
      ref      = remote$ref,
      pat      = remote$auth_token %||% remotes:::github_pat(),
      use_curl = use_curl
    ),
    http_422 = function(e) {
      NA_character_
    },
    http_403 = function(e) {
      NA_character_
    }
  )
}

jaspBase:::assignFunctionInPackage(remote_sha.github_remote2, "remote_sha.github_remote", "remotes")
