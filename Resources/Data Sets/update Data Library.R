# create the basic entries
index <- list(name = "Categories",
                 path = "Data Library",
                 description = "General Examples",
                 kind = "folder",
                 children = list())


# read descriptions (run to duplicate files to Data Library/Books)
descriptions <- read.csv("JASP Examples Description.csv", header = TRUE, stringsAsFactors = FALSE)
  
allpaths <- list.files("Data Library", recursive = TRUE, full.names = TRUE)
allfiles <- gsub(".*/*/", "", allpaths)
allnames <- gsub("*.(csv|jasp)", "", allfiles)

allOrigin <- descriptions$Origin[match(allnames, descriptions$Chapter)]
isInField <- grep(pattern = "from Andy Field", x = allOrigin)  
isInMoore <- grep(pattern = "from Moore, McCabe, & Craig", x = allOrigin)

# copy files to books
# file.copy(allpaths[isInField], 
#          to = paste0("Data Library/Books/Field - Discovering Statistics/", allfiles[isInField]),
#           overwrite = TRUE)
# file.copy(allpaths[isInMoore],
#           to = paste0("Data Library/Books/Moore, McCabe, & Craig - Introduction to the Practice of Statistics/",
#                       allfiles[isInMoore]),
#           overwrite = TRUE)




# recursive function to create kind = folder
createChildren <- function(folder){
  folders <- gtools::mixedsort(list.dirs(folder,
                                         full.names = FALSE,
                                         recursive = FALSE))
  
  # Books should be first in the main menu
  if("Books" %in% folders){
    folders <- folders[c(length(folders), 1:(length(folders)-1))]
    description <- rep("", length(folders))
  
  # Create license description for the books!    
  } else{
  description <- descriptions[descriptions$Type == "Folder",]
  description <- paste(description$Description.for.the.JASP.files, 
                       "<br><br>", description$Origin)
  }
  

  # create folders
  chapters <- data.frame(
    name = folders,
    path = folders,
    description = description,
    kind = rep("folder", length(folders)),
    stringsAsFactors = FALSE
  )
  
  # create chilren of the folders
  folderpaths <- gtools::mixedsort(list.dirs(folder,
                                             full.names = TRUE,
                                             recursive = FALSE))
  
  if("Books" %in% folders){
    folderpaths <- folderpaths[c(length(folderpaths), 1:(length(folderpaths)-1))]
  }
  
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
  txtfiles <- dir(folder, recursive = FALSE,
                  include.dirs = FALSE,
                  full.names = FALSE,
                  pattern = "(license|.txt)")

  # if the folder contains folders, and not files, create the folders
  if(length(jaspfiles)==0 && length(csvfiles) == 0 && length(txtfiles) == 0){
    return(createChildren(folder))
  # if the folder contains files, create them  
  } else if (length(jaspfiles)==0 && length(csvfiles) == 0 && length(txtfiles) != 0){
    return(data.frame(
      name = "",
      path = "",
      description = "",
      kind = "",
      stringsAsFactors = FALSE
    ))
  } else if (length(jaspfiles) != 0){
    name <- gsub(".jasp", "", jaspfiles)
    
    # load a description
    description <- descriptions[match(name, descriptions$Chapter), "Description.for.the.JASP.files"]
    description <- paste(description, "<br><br>")
    
    # load analysis 
    analysis<- descriptions[match(name, descriptions$Chapter), "Analysis2"]
    analysis <- paste("The example JASP file demonstrates the use of ", analysis,".<br><br>", sep = "")
  
    # load the source information
    origin <- descriptions[match(name, descriptions$Chapter), "Origin"]
    origin <- paste0("<i>", origin, "</i>")
  
    
      jaspfiles <- data.frame(
        name = gsub(".jasp", "", jaspfiles),
        path = jaspfiles,
        description = paste(description, analysis, origin),
        kind = "file",
        stringsAsFactors = FALSE
        )

    # associate jasp files with csv files
    csvfiles <- data.frame(
      name = gsub(".csv", "", csvfiles),
      associated_datafile = csvfiles,
      stringsAsFactors = FALSE
    )
    
    entries <- dplyr::full_join(jaspfiles, csvfiles, by = "name")
    entries$children <- NULL
    return(entries)
  } else if(length(csvfiles) != 0){
    name <- gsub(".csv", "", csvfiles)
    
    # load a description
    description <- descriptions[match(name, descriptions$Chapter), "Description.for.the.JASP.files"]
    description <- paste(description, "<br><br>")
    
    # load the source information
    origin <- descriptions[match(name, descriptions$Chapter), "Origin"]
    origin <- paste0("<i>", origin, "</i>")
    
    
    csvfiles <- data.frame(
      name = gsub(".csv", "", csvfiles),
      path = csvfiles,
      description = paste(description, origin),
      kind = "file",
      stringsAsFactors = FALSE
    )
    
    entries <- csvfiles
    entries$children <- NULL
    return(entries)
  }  
}

# create the children of Data Library
DataLibrary <- createEntries("Data Library")


# Andy field books
fieldnames <- unique(allnames[isInField])
fieldjaspPaths <- paste0("../../../", allpaths[isInField][grep(".jasp", allpaths[isInField])])
fieldcsvPaths <- paste0("../../../", allpaths[isInField][grep(".csv", allpaths[isInField])])
fieldDescriptions <- descriptions[match(fieldnames, descriptions$Chapter), "Description.for.the.JASP.files"]
fieldDescriptions <- paste(fieldDescriptions, "<br><br>")
fieldOrigin <- descriptions[match(fieldnames, descriptions$Chapter), "Origin"]
fieldOrigin <- paste0("<i>", fieldOrigin, "</i>")

DataLibrary[['children']][[1]][['children']][[1]] <- data.frame(name = fieldnames,
                                                                path = fieldjaspPaths,
                                                                description = paste(fieldDescriptions, fieldOrigin),
                                                                kind = rep("file", length(fieldnames)),
                                                                associated_datafile = fieldcsvPaths)
DataLibrary[['children']][[1]][['children']][[1]] <- DataLibrary[['children']][[1]][['children']][[1]][order(fieldnames),]

# Moore et al books
moorenames <- unique(allnames[isInMoore])
moorejaspPaths <- paste0("../../../", allpaths[isInMoore][grep(".jasp", allpaths[isInMoore])])
moorecsvPaths <- paste0("../../../", allpaths[isInMoore][grep(".csv", allpaths[isInMoore])])
mooreDescriptions <- descriptions[match(moorenames, descriptions$Chapter), "Description.for.the.JASP.files"]
mooreDescriptions <- paste(mooreDescriptions, "<br><br>")
mooreOrigin <- descriptions[match(moorenames, descriptions$Chapter), "Origin"]
mooreOrigin <- paste0("<i>", mooreOrigin, "</i>")

DataLibrary[['children']][[1]][['children']][[2]] <- data.frame(name = moorenames,
                                                                path = moorejaspPaths,
                                                                description = paste(mooreDescriptions, mooreOrigin),
                                                                kind = rep("file", length(moorenames)),
                                                                associated_datafile = moorecsvPaths)
DataLibrary[['children']][[1]][['children']][[2]] <- DataLibrary[['children']][[1]][['children']][[2]][order(moorenames),]

index$children <- DataLibrary
#index <- data.frame(index, stringsAsFactors = FALSE)

# include debug data set
debugdata <- data.frame(name = "Debug Dataset (JASP Team, 2017)",
                        path = "../debug.csv",
                        description = "For testing. Readme: is.gd/jaspdata",
                        kind = "file",
                        children = "NULL",
                        debug ="true")


index$children$debug <- "false"
index$children <- rbind(index$children, debugdata)

# create jsons
jsonIndex <- jsonlite::toJSON(index, pretty = TRUE)
write(jsonIndex, "index.json")

############################
#####  DO NOT FORGET #######
# remove "[]" from the index names - will not find the paths if it's there.


# To load the JASP examples classic 
# index[['children']] <- data.frame(
#   name = c("Data Library", 
#            "Political Democracy (Bollen, 1989)",
#            "Bugs (Ryan, Wilde & Crist, 2013)",
#            "Tooth Growth",
#            "Kitchen Rolls",
#            "Big 5 (Dolan, Oort, Stoel & Wicherts, 2009)",
#            "Debug Dataset (JASP Team, 2017)"),
#   path = c("Data Library", 
#            "Political Democracy (Bollen, 1989).csv",
#            "Bugs (Ryan, Wilde & Crist, 2013).csv",
#            "Tooth Growth.csv",
#            "Kitchen Rolls.csv",
#            "Big 5 (Dolan, Oort, Stoel & Wicherts, 2009).csv",
#            "debug.csv"),
#   description = c("Data Library",
#                   "A SEM data set",
#                   "A repeated measures ANOVA data set",
#                   "An uninspiring ANOVA data set",
#                   "A nice t-test data set",
#                   "A nice correlation data set",
#                   "For testing. Readme: is.gd/jaspdata"),
#   kind = c("folder",
#            "file",
#            "file",
#            "file",
#            "file",
#            "file",
#            "file"),
#   stringsAsFactors = FALSE
# )

