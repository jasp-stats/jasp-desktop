#' @export
lintAnalysis <- function(analysis) {

  nLint <- length(analysis)
  output <- vector("list", nLint)
  prog <- dplyr::progress_estimated(nLint)
    # lintr checks the parent environment for function definitions (i.e.)
  env <- new.env()
  if (.isModule())
    .sourceDir(.getPkgOption("module.dir"), env)
  else
    .sourceDir(.getPkgOption("common.r.dir"), env)

  for (i in seq_len(nLint)) {

    analysis[i] <- try(.validateAnalysis(analysis[i]), silent = FALSE)
    if (.isModule())
      filename <- file.path(.getPkgOption("module.dir"), "R", paste0(analysis[i], ".R"))
    else
      filename <- file.path(.getPkgOption("common.r.dir"), paste0(analysis[i], ".R"))
    output[[i]] <- if (!inherits("try-error", analysis[i])) {
      lintr::lint(
        filename = filename,
        linters = list(
          absolute_paths_linter          = lintr::absolute_path_linter,
          no_tab_linter                  = lintr::no_tab_linter,
          assignment_linter              = lintr::assignment_linter,
          line_length_linter             = lintr::line_length_linter(120L),
          trailing_whitespace_linter     = lintr::trailing_whitespace_linter,
          spaces_inside_linter           = lintr::spaces_inside_linter,
          open_curly_linter              = lintr::open_curly_linter,
          object_name_linter             = lintr::object_name_linter,
          commas_linter                  = lintr::commas_linter,
          infix_spaces_linter            = lintr::infix_spaces_linter,
          T_and_F_symbol_linter          = lintr::T_and_F_symbol_linter,
          object_usage_linter            = object_usage_linter_custom(env),
          object_length_linter           = lintr::object_length_linter,
          undesirable_operator_linter    = lintr::undesirable_operator_linter,
          unneeded_concatenation_linter  = lintr::unneeded_concatenation_linter,
          spaces_left_parentheses_linter = lintr::spaces_left_parentheses_linter,
          undesirable_function_linter    = lintr::undesirable_function_linter
        )
      )
    } else {
      analysis[i]
    }
    prog$tick()$print()
  }
  class(output) <- "jaspLint"
  return(output)
}

#' @export
lintAll <- function() {

  env <- new.env()
  if (.isModule()) {
    .sourceDir(.getPkgOption("module.dir"), env)
    files <- list.files(file.path(.getPkgOption("module.dir"), "R"), "\\.[RrSsQq]$")
  } else {
    .sourceDir(.getPkgOption("common.r.dir"), env)
    files <- list.files(.getPkgOption("common.r.dir"), "\\.[RrSsQq]$")
  }
  output <- vector("list", length(files))
  names(output) <- files

  prog <- dplyr::progress_estimated(length(files))
  for (f in files) {
    if (.isModule())
      filename <- file.path(.getPkgOption("module.dir"), "R", f)
    else
      filename <- file.path(.getPkgOption("common.r.dir"), f)
    output[[f]] <- lintr::lint(
      filename = filename,
      linters = list(
        absolute_paths_linter          = lintr::absolute_path_linter,
        no_tab_linter                  = lintr::no_tab_linter,
        assignment_linter              = lintr::assignment_linter,
        line_length_linter             = lintr::line_length_linter(120L),
        trailing_whitespace_linter     = lintr::trailing_whitespace_linter,
        spaces_inside_linter           = lintr::spaces_inside_linter,
        open_curly_linter              = lintr::open_curly_linter,
        object_name_linter             = lintr::object_name_linter,
        commas_linter                  = lintr::commas_linter,
        infix_spaces_linter            = lintr::infix_spaces_linter,
        T_and_F_symbol_linter          = lintr::T_and_F_symbol_linter,
        object_usage_linter            = object_usage_linter_custom(env),
        object_length_linter           = lintr::object_length_linter,
        undesirable_operator_linter    = lintr::undesirable_operator_linter,
        unneeded_concatenation_linter  = lintr::unneeded_concatenation_linter,
        spaces_left_parentheses_linter = lintr::spaces_left_parentheses_linter,
        undesirable_function_linter    = lintr::undesirable_function_linter
      )
    )
    prog$tick()$print()
  }
  class(output) <- "jaspLint"
  return(output)
}

#' @export
testLint <- function(lintObj = NULL) {

  if (is.null(lintObj))
    lintObj <- lintAll()


}

#' @export
styleAnalysis <- function(analysis, silent = FALSE, safetyCopy = tempfile(fileext = ".R")) {

  if (inherits(analysis, "lints")) {
    obj <- analysis
    fullname <- names(analysis)[1]
    filename <- basename(fullname)
  } else {
    filename <- paste0(.validateAnalysis(analysis), ".R")
    if (.isModule())
      fullname <- file.path(.getPkgOption("module.dir"), "R", filename)
    else
      fullname <- file.path(.getPkgOption("common.r.dir"), filename)
    obj <- lintAnalysis(analysis)
  }

  if (!file.copy(from = fullname, to = safetyCopy, overwrite = FALSE))
    stop("Could not create safety copy. Aborting styling.")
  if (!silent)
    message(sprintf("A safety-copy is available at %s", safetyCopy))

  if (silent) {
    utils::capture.output(
      {
        obj <- styler::style_file(
          path                          = fullname,
          scope                         = "tokens",
          strict                        = FALSE,
          start_comments_with_one_space = TRUE
        )
      }
    )
  } else {
    obj <- styler::style_file(
      path                          = fullname,
      scope                         = "tokens",
      strict                        = FALSE,
      start_comments_with_one_space = TRUE
    )
  }
  return(invisible(obj))
}

# custom lintr checks go here ----
object_usage_linter_custom <- function(parent_env = NULL) {

  function(source_file) {
    if (is.null(parent_env)) {
      pkg_name <- pkg_name(lintr:::find_package(dirname(source_file$filename)))
      if (!is.null(pkg_name)) {
        parent_env <- lintr:::try_silently(getNamespace(pkg_name))
      }
      if (is.null(pkg_name) || inherits(parent_env, "try-error")) {
        parent_env <- globalenv()
      }
    }
    env <- new.env(parent = parent_env)

    globals <- mget(".__global__", parent_env, ifnotfound = list(NULL))$.__global__
    mapply(assign, globals, MoreArgs = list(value = function(...) NULL,
                                            envir = env))
    lintr:::try_silently(eval(source_file$parsed_content, envir = env))
    all_globals <- unique(lintr:::recursive_ls(env))
    # lapply(lintr:::ids_with_token(source_file, rex::rex(start, "FUNCTION"), fun = rex::re_matches),
    lapply(lintr:::ids_with_token(source_file, "^FUNCTION", fun = rex::re_matches),
           function(loc) {
             id <- source_file$parsed_content$id[loc]
             parent_ids <- lintr:::parents(source_file$parsed_content, id,
                                           simplify = FALSE)
             if (length(parent_ids) > 3L) {
               return(NULL)
             }
             fun <- lintr:::try_silently(eval(parse(text = source_file$content,
                                                    keep.source = TRUE), envir = env))
             if (inherits(fun, "try-error")) {
               return()
             }
             res <- lintr:::parse_check_usage(fun)
             locals <- codetools::findFuncLocals(formals(fun), body(fun))
             both <- c(locals, names(formals(fun)), all_globals)
             lapply(which(!is.na(res$message)), function(row_num) {
               row <- res[row_num, ]
               if (rex::re_matches(row$message, rex::rex("no visible"))) {
                 suggestion <- lintr:::try_silently(both[stringdist::amatch(row$name,
                                                                            both, maxDist = 2)])
                 if (!inherits(suggestion, "try-error") && !is.na(suggestion)) {
                   row$message <- paste0(row$message, ", Did you mean '",
                                         suggestion, "'?")
                 }
               }
               org_line_num <- as.integer(row$line_number) + as.integer(names(source_file$lines)[1]) -
                 1L
               line <- source_file$lines[as.character(org_line_num)]
               row$name <- rex::re_substitutes(row$name, rex::rex("<-"), "")
               location <- rex::re_matches(line, rex::rex(row$name), locations = TRUE)
               lintr:::Lint(filename = source_file$filename, line_number = org_line_num,
                            column_number = location$start, type = "warning",
                            message = row$message, line = line, ranges = list(c(location$start,
                                                                                location$end)), linter = "object_usage_linter")
             })
           })
  }
}
