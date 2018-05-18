simple <- commandArgs(trailingOnly = TRUE)
simple <- ifelse(length(simple) == 0, FALSE, TRUE)

# create the basic entries
index <- list(name = "Categories",
                 path = "Examples",
                 description = "General Examples",
                 kind = "folder",
                 children = list())

# To load the JASP examples classic 
index[['children']] <- data.frame(
  name = c("Data Library", 
           "Political Democracy (Bollen, 1989)",
           "Bugs (Ryan, Wilde & Crist, 2013)",
           "Tooth Growth",
           "Kitchen Rolls",
           "Big 5 (Dolan, Oort, Stoel & Wicherts, 2009)",
           "Debug Dataset (JASP Team, 2017)"),
  path = c("Data Library", 
           "Political Democracy (Bollen, 1989).csv",
           "Bugs (Ryan, Wilde & Crist, 2013).csv",
           "Tooth Growth.csv",
           "Kitchen Rolls.csv",
           "Big 5 (Dolan, Oort, Stoel & Wicherts, 2009).csv",
           "debug.csv"),
  description = c("Example Data Files",
                  "A SEM data set",
                  "A repeated measures ANOVA data set",
                  "An uninspiring ANOVA data set",
                  "A nice t-test data set",
                  "A nice correlation data set",
                  "For testing. Readme: is.gd/jaspdata"),
  kind = c("folder",
           "file",
           "file",
           "file",
           "file",
           "file",
           "file"),
  stringsAsFactors = FALSE
)

# if input says simple, we should generate debug .json only for the classic datasets
if(simple){
  cat("Creating Simple indexdebug.json")
  jsonIndex <- jsonlite::toJSON(index, pretty = TRUE)
  write(jsonIndex, "indexdebug.json")
}else{
  cat("Creating full index.json and indexdebug.json")
  # recursive function to create kind = folder
  createChildren <- function(folder){
    folders <- gtools::mixedsort(list.dirs(folder,
                                           full.names = FALSE,
                                           recursive = FALSE))
    
    chapters <- data.frame(
      name = folders,
      path = folders,
      description = rep("", length(folders)),
      kind = rep("folder", length(folders)),
      stringsAsFactors = FALSE
    )
    
    folderpaths <- gtools::mixedsort(list.dirs(folder,
                                               full.names = TRUE,
                                               recursive = FALSE))
    chapters$children <- plyr::llply(folderpaths, createEntries)
    return(chapters)
  }
  
  # recursive function to create kind = file
  createEntries <- function(folder){
    
    jaspfiles <- gtools::mixedsort(dir(folder,
                                       recursive = FALSE,
                                       include.dirs = FALSE,
                                       full.names = FALSE,
                                       pattern = ".jasp"))
    csvfiles <- gtools::mixedsort(dir(folder,
                                      recursive = FALSE,
                                      include.dirs = FALSE,
                                      full.names = FALSE,
                                      pattern = ".csv"))
    
    if(length(jaspfiles)==0){
      return(createChildren(folder))
    } else{
    
      jaspfiles <- data.frame(
        name = gsub(".jasp", "", jaspfiles),
        path = jaspfiles,
        description = jaspfiles,
        kind = "file",
        stringsAsFactors = FALSE
      )
      
      csvfiles <- data.frame(
        name = gsub(".csv", "", csvfiles),
        associated_datafile = csvfiles,
        stringsAsFactors = FALSE
      )
      
      entries <- dplyr::full_join(jaspfiles, csvfiles, by = "name")
      entries$children <- NULL
      return(entries)
    }  
  }
  
  # chreate the children of Data Library
  DataLibrary <- createEntries("Data Library")
  index[['children']][['children']] <- ifelse(index[['children']][['kind']] == "folder",
                                              list(DataLibrary),
                                              NA)
  
  
  
  # create jsons
  jsonIndex <- jsonlite::toJSON(index, pretty = TRUE)
  write(jsonIndex, "indexdebug.json")
  
  index$children <- index$children[-nrow(index$children),]
  
  jsonIndex <- jsonlite::toJSON(index, pretty = TRUE)
  write(jsonIndex, "index.json")
}