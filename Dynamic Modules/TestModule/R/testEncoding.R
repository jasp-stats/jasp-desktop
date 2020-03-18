testEncodingFunc <- function(jaspResults, dataset, options)
{
  cols <- unlist(options$selected)
  dat  <- .readDataSetToEnd(columns=cols)
  tab  <- createJaspTable("Data & Encoding")

  jaspResults[["encodingsEtc"]] <- tab

  print(dat)

  encName <- function(col) { return(paste0(col, "_encoding")); }
  filName <- function(col) { return(paste0(col, "_filtered")); }

  for(col in cols)
  {
    print(paste0("Working on col: ", col))

    tab$addColumnInfo(name=col)
    colDat      <- dat[[.v(col)]]
    tab[[col]]  <- as.character(colDat)

    tab$addColumnInfo(name=encName(col))
    tab[[encName(col)]] <- stringi::stri_enc_mark(colDat)

    tab$addColumnInfo(name=filName(col))
    tab[[filName(col)]] <- eval(parse(text=paste0("colDat ", options$genericFilter)))   
  }
}
