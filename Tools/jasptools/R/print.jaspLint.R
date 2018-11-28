errorCol   <- crayon::combine_styles("red", "bold")
warningCol <- crayon::make_style(rgb(5/7, 2/7, 0), bg = FALSE)
goodCol    <- crayon::green

# @export
print.jaspLint <- function(x, showmax = 3, ...) {

  errorList <- vector("list", length(x))
  shouldBeFixed <- FALSE
  for (i in seq_along(x)) {

    current <- x[[i]]
    errors <- list()

    name <- basename(current[[1]]$filename)
    cat(paste0(
      strrep("=", nchar(name)), "\n",
      name, "\n",
      strrep("=", nchar(name)), "\n",
      collapse = ""
    ))
    for (j in seq_along(current)) {
      errorType <- current[[j]][["message"]]

      idx <- match(errorType, names(errors), nomatch = NA)
      if (!is.na(idx)) {
        errors[[idx]] <- c(errors[[idx]], current[[j]]$line_number)
      } else {
        idx <- length(errors) + 1L
        errors[[idx]] <- current[[j]]$line_number
        names(errors)[idx] <- errorType
      }
    }
    if (length(errors) > 0) {

      nms <- names(errors)
      nms <- substr(nms, 1, nchar(nms) - 1L) # cut off trailing .

      # show errors first then warnings. Sort each alphabetically
      o <- order(nms)
      idx <- nms[o] %in% .listOfFatalStylesErrors
      order4loop <- c(o[idx], o[!idx])

      for (j in order4loop) {

        toShow  <- errors[[j]]
        toShow  <- toShow[1:min(length(toShow), showmax)]
        andMore <- length(errors[[j]]) - length(toShow)

        line1 <- if (length(toShow) > 1) "lines" else "line"
        line2 <- if (andMore > 1) "lines" else "line"

        if (andMore > 0) {
          string <- sprintf("%s on %s: %s, and %d more %s.\n",
                            nms[j], line1, paste(toShow, collapse = ", "), andMore, line2)
        } else {
          string <- sprintf("%s on %s: %s.\n",
                            nms[j], line1, paste(toShow, collapse = ", "))
        }

        if (nms[j] %in% .listOfFatalStylesErrors) {
          shouldBeFixed <- TRUE
          cat(errorCol(string))
        } else {
          cat(warningCol(string))
        }
      }
    } else {
      cat(goodCol("Perfect!"))
    }
    cat("\n")
    errorList[[i]] <- errors
  }
  return(invisible(list(errorList = errorList, shouldBeFixed = shouldBeFixed)))
}

.listOfFatalStylesErrors <- c(
  "Commas should always have a space after",
  "Commas should never have a space before",
  "Do not place spaces around code in parentheses or square brackets",
  "Place a space before left parenthesis, except in a function call",
  "Put spaces around all infix operators", "Unneded concatenation of a constant. Remove the \"c\" call",
  "Unneded concatenation without arguments. Replace the \"c\" call by NULL or vector()",
  "Use <-, not =, for assignment", "Use spaces to indent, not tabs",
  "Use FALSE instead of the symbol F"
)
