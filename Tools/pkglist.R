args <- commandArgs(trailingOnly = TRUE)

if (length(args)==0) {
  cat("\nArguments:\n[LIBRARY] (relative path to folder)\n[EDIT_PKG_CHECK] (boolean, edit packagecheck.R?)\n[OUTPUT_LOCATION] (relative path to folder)\n[COLUMNS...]\n\nFor available columns run: Rscript pkglist.r -wc\n")
} else if (length(args) == 1) {
  if (args[1] == "-wc"){
    cat("Columns available:Package\nVersion\nPriority\nDepends\nImports\nLinkingTo\nSuggests\nEnhances\nLicense\nLicense_is_FOSS\nLicense_restricts_use\nOS_type\nMD5sum\nNeedsCompilation\nBuilt\nMaintainer\nAuthor\nDescription\nTitle\nPackaged\nRepository\nDate/Publication\nRemoteType\nRemoteUrl\nType\nURL\nEncoding\nVignetteBuilder\nAuthors@R\nDate\nLazyLoad\nX-CRAN-Original-Package\nX-CRAN-Original-Maintainer\nX-CRAN-Comment\nBugReports\nNote\nLazyData\nByteCompile\nRepository/R-Forge/Project\nRepository/R-Forge/Revision\nRepository/R-Forge/DateTimeStamp\nSuggestsNote\nBuildResaveData\nSystemRequirements\nCopyright\nBiarch\nCollate\nContact\nBuildVignettes\nAdditional_repositories\nRevision\nLazyDataNote\nReferences\nZipData\nRoxygen\nMailingList\nRoxygenNote\nBuildManual")
  } else {
    cat("\nArguments:\n[LIBRARY] (relative path to folder)\n[OUTPUT_LOCATION] (relative path to folder)\n[COLUMNS...]\n\nFor available columns run: Rscript pkglist.r -wc\n")
  }
} else if (length(args) >= 2) {
  
  addcol <- function(out, col) {
    n <- data.frame(out, unlist(lapply(rownames(out), FUN = function(x) {
      c <- packageDescription(x)[[col]]
      if (is.null(c)) c <- ""
      c
    })), stringsAsFactors = FALSE)
    colnames(n) <- c(colnames(out), col)
    return(n)
  }
  # Generates a list of packages from a build library
  libdir <- paste0(getwd(),"/",args[1])

  if (!dir.exists(libdir)){
    stop("Library directory not found")
  }

  if (is.na(args[3])) {
    if (tolower(args[2]) == "false") {
      stop("Please specify the out folder")
    }
  } else {
    if (tolower(args[2]) == "true") {
      warning("Ignoring out folder and columns --- editing packagecheck.R\n")
    } else {
      outlocation <- paste0(getwd(),"/",args[3])
      if (!dir.exists(outlocation)) {
        stop("Out folder not found")
      }
    }
  }

  .libPaths(libdir)
  i <- installed.packages(.libPaths()[1])

  editPkgCheck <- ifelse(tolower(args[2]) == "true", TRUE, FALSE)
  if (editPkgCheck) {
    selcols <- c("Package", "Version")
  } else {
    selcols <- args[-(1:3)]
  }
  
  if (length(selcols) == 0) {
    stop("No columns specified")
  }

  if (all(selcols %in% colnames(i))) {
    out <- data.frame(i[,selcols], stringsAsFactors = F)
  } else if (all(selcols %in% unique(c(colnames(i), "Package","Maintainer",
                                       "Version","Author","Description","Title",
                                       "License","Packaged","Repository",
                                       "Date/Publication","NeedsCompilation",
                                       "Built","RemoteType","RemoteUrl","Type",
                                       "Depends","Suggests","Imports","URL",
                                       "Encoding","VignetteBuilder","Authors@R",
                                       "Date","Priority","LazyLoad",
                                       "X-CRAN-Original-Package",
                                       "X-CRAN-Original-Maintainer",
                                       "X-CRAN-Comment","BugReports","LinkingTo",
                                       "Note","LazyData","ByteCompile",
                                       "Repository/R-Forge/Project",
                                       "Repository/R-Forge/Revision",
                                       "Repository/R-Forge/DateTimeStamp",
                                       "SuggestsNote","BuildResaveData",
                                       "Enhances","SystemRequirements",
                                       "Copyright","Biarch","Collate","Contact",
                                       "BuildVignettes","Additional_repositories",
                                       "Revision","LazyDataNote","References",
                                       "ZipData","Roxygen","MailingList",
                                       "RoxygenNote","BuildManual")))) {
    direct <- which(selcols %in% colnames(i))
    if (length(direct) == 0) {
      out <- data.frame(i[,c("Package")], stringsAsFactors = F)
      for (col in selcols) {
        out <- addcol(out,col)
      }
      out <- out[, -1]
    } else {
      out <- data.frame(i[, selcols[direct]], 
                        stringsAsFactors = F)
      colnames(out) <- selcols[direct]
      for (col in selcols[-direct]) {
        out <- addcol(out, col)
      }
    }
    
    
  } else {
    stop("Some columns not found. Run Rscript pkglist.r -wc for available columns.\nNB: pay attention to capitalisation!")
  }

  if (editPkgCheck) {
    locPkgCheck <- file.path("..", "JASP-Engine", "jaspBase", "R", "packagecheck.R")
    if (! file.exists(locPkgCheck))
      stop(paste("Could not locate packagecheck.R at", locPkgCheck))
    
    oldScript <- readLines(locPkgCheck)
    indicators <- sapply(oldScript, function(x) grepl("#--auto-generated", x, fixed=TRUE))
    start <- which(indicators)[1]
    end <- which(indicators)[2]
    if (any(c(start, end) == integer(0)))
      stop("Could not locate markers in packagecheck.R")
    
    outStr <- character(nrow(out))
    for (row in 1:nrow(out)) {
      outStr[row] <- paste0("\texpected <- rbind(expected, c('", out[row, 1], "', '", out[row, 2], "'))")
    }
    script <- oldScript[1:start]
    script <- c(script, outStr)
    script <- c(script, oldScript[end:length(oldScript)])
    
    write(script, file=locPkgCheck)
    cat("Updated packagecheck.R\n")
  }

  if (exists("outlocation")) {
    write.csv(out, paste0(outlocation, "/pkglist.csv"), row.names = FALSE)
    cat("File", paste0(outlocation, "/pkglist.csv"), "saved!\n")
  }
}