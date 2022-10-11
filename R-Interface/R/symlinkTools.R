
# Goal of this script:
# Go through all the module folders
# Collect all the symlinks
# Know renv-cache path
# Subtract renv-cache path from the symlinks to go to relative paths
# Unix:
#   Recreate the symlinks directly on unix but relative this time
# Win:
#   Save them to a file that can be used to restore them on Windows
#   Have a function here or in that file that restores them and add it as a custom action to Wix

pastePath <- function(path) { return(paste0(  path, collapse=.Platform$file.sep)     ) }
splitPath <- function(path) 
{ 
  path  <- gsub("\\", "/", path, fixed=TRUE)
  paths <- strsplit(path, "/")[[1]]; 
  return(paths[paths != ""]) # Remove "empty" dir between // like: "blablac//weird/path"
}

# This returns two functions that can be used to convert paths in two direcitons
determineOverlap <- function(targetRoot, sourceRoot)
{
  targetSplit <- splitPath(targetRoot)
  sourceSplit <- splitPath(sourceRoot)
  len         <- min(length(targetSplit), length(sourceSplit))
  overlap     <- 0

  for(idx in seq_len(len))
    if(sourceSplit[[idx]] == targetSplit[[idx]]) overlap <- idx
    else                                         break

  overlapVec <- targetSplit[seq(overlap)]
  overlap    <- list(
    vec = overlapVec, 
    str = pastePath(overlapVec),
    len = overlap
  )

  # This function returns the path from the target location to the source as seen from target (aka "(../)*" with either from the root to the source added or not depending on logical addRootToSource
  targetToSource <- function(target, addRootToSource)
  {
    targetSplit  <- splitPath(target)
    rootToSrc    <- pastePath(sourceSplit[seq(overlap$len + 1, length(sourceSplit))])
    stepsDown    <- length(targetSplit) - (overlap$len + as.integer(addRootToSource))
    tgtToSrc     <- pastePath(rep("..", stepsDown)  )

    #for debug:
    #tgtToSrc     <- paste0(tgtToSrc, .Platform$file.sep, ".")

    if(addRootToSource)
      return(paste0(tgtToSrc, rootToSrc)) #We do not need to add the separator because it is there in tgtToSrc
    return(tgtToSrc)
  }

  #This one returns the path from the root (overlap) to where target is.
  sourceToTarget <- function(target)
  {
    targetSplit  <- splitPath(target)
    srcToTgt     <- pastePath(targetSplit[seq(overlap$len + 1, length(targetSplit))])

    return(srcToTgt)
  }

  return(list(targetToSource=targetToSource, sourceToTarget=sourceToTarget))
}

# Use overlapfunctions as returned by determineOverlap to generate a function to turn target-path from absolute to relative
getRelativityFunction <- function(modulesRoot, renvCache)
{
  if (Sys.info()["sysname"] == "Darwin") {
    #Lets make the hack better, if R.framework is in here we are installing jaspBase & co, else: jaspModules & co
    if(grepl("R.framework", modulesRoot)) modToRenvS <- pastePath(c(rep("..",6), "Modules", "renv-cache"))
    else                                  modToRenvS <- pastePath(c("..", "renv-cache"))
  } else {
    modToRenvF <- determineOverlap(modulesRoot, renvCache)
    modToRenvS <- modToRenvF$targetToSource(renvCache, TRUE)
  }

  return(
    function(linkLocation, targetPath)
    {
      linkLocation <-              (path.expand(linkLocation)) #Do not normalize it because Windows then follows this path...
      targetPath   <- normalizePath(path.expand(targetPath))
      linkToMod    <- determineOverlap(linkLocation, modulesRoot)$targetToSource
      pathToRenv   <- determineOverlap(targetPath,   renvCache)$sourceToTarget

      linkToModS   <- linkToMod(linkLocation, FALSE)
      linkToRenvS  <- modToRenvS #pastePath(c(linkToModS, modToRenvS))
      pathToRenvS  <- pathToRenv(targetPath)

      newTarget    <- paste0(linkToRenvS, .Platform$file.sep, pathToRenvS)

      # print(paste0("for link '", linkLocation, "' and target '",targetPath, "'"))
      # print(paste0("- linkToModS '",linkToModS, " modToRenvS: '", modToRenvS, "' pathToRenvS: '", pathToRenvS, "'\n results in newTarget: '", newTarget, "'"))
      
      return(newTarget)
    }
  )
}

 # copy paste from https://stat.ethz.ch/R-manual/R-patched/library/base/html/Sys.readlink.html
is.symlink  <- function(paths) isTRUE(nzchar(Sys.readlink(paths), keepNA=TRUE))

# See https://stackoverflow.com/questions/2311105/test-in-powershell-code-if-a-folder-is-a-junction-point because there is no R equivalent...
# these functions are also really quite slow... But ok, it is only necessary to run it during building.
is.junction <- function(paths)
{
  #Also this function isn't very safe, make sure that you only ask it about existing paths or you get garbage
  as.logical(
    sapply(
      paths, 
      function(path) 
        as.logical(as.integer(
          system2(command="powershell", args=paste0('-command "if ((Get-Item -Path ', path,' -Force).LinkType -eq \\"Junction\\") { 1 } else { 0 }'), stdout=TRUE)
        )) 
    )
  )
}

get.junction.pwrshll <- function(paths) 
{
  sapply(
    paths, 
    function(path) system2(command="powershell", args=paste0('-command "(Get-Item -Path ', path,' -Force).Target'), stdout=TRUE)
)
}
 


# Returns a list of symlinks with target location relative to modulesRoot
collectLinks <- function(modulesRoot, renvCache, isLink, getLink)
{
  # Honestly this whole recursive setup for determining the shared overlap of the renv-cache
  # and Modules is totally overkill as they are always next to each other. But I wanted to
  # allow for the possibility of moving the renv-cache somewhere else and the code works fine
  # now, so... I'll leave it like this. The next time someone needs to work on this and this
  # complexity is causing trouble we should know whether or not it is useful to have this
  # flexibility or not. And remove it if not.
  modulesRoot <- normalizePath(path.expand(modulesRoot))
  renvCache   <- normalizePath(path.expand(renvCache))

  print(paste0("modulesRoot: '", modulesRoot, "' and renvCache: '", renvCache, "'"))

  #setwd(modulesRoot)

  # Sometimes a dutch word just works so much better than english, so here `relativeer > relativize`
  relativeer <- getRelativityFunction(modulesRoot, renvCache)
  symlinks   <- data.frame(linkLocation=character(0), linkTarget=character(0), originalTarget=character(0))

  collectSymlinks <- function(paths)
    for(path in paths)
    {
      #path <- normalizePath(path) This follows the junction immediately...
      if(!grepl("renv-cache", path)){
            #print(paste0("Checking if path is link: ", path))
            if(isLink(path))
            {
              #print('is symlink')
              symPath  <- getLink(path)
              if(!startsWith(symPath, ".")) #if starts with dot it is already relative
                symlinks[nrow(symlinks)+1, ] <<- list(linkLocation=path, linkTarget=relativeer(path, symPath), originalTarget=symPath)
            }
            else
            {
              everything  <- list.files(path, recursive=FALSE, include.dirs=TRUE, all.files=FALSE, full.names=TRUE)
      
              if(length(everything) > 0)
              {
                allDirs     <- everything[file.info(everything)$isdir]
                collectSymlinks(allDirs)
              }
            }
        }
    }

  collectSymlinks(modulesRoot)

  # print("Found symlinks:")
  # print(symlinks)

  return(symlinks)
}


generatePadFunction <- function()
{
  #little helper to make the log output easier to read
    printSizes <- integer(3)
    padToMax <- function(str, idx)
    {
      printSizes[[idx]] <<- max(nchar(str), printSizes[[idx]])
      needThisMany      <-  printSizes[[idx]] - nchar(str)

      return(paste0(str, strrep(" ", needThisMany)))
    }
}

#call like: convertAbsoluteSymlinksToRelative("~/Broncode/build-JASP-Desktop_Qt_5_15_2_clang_64bit-Debug/Modules", "~/Broncode/build-JASP-Desktop_Qt_5_15_2_clang_64bit-Debug/renv-cache")
convertAbsoluteSymlinksToRelative <- function(modulesRoot, renvCache)
{
  symlinks <- collectLinks(modulesRoot, renvCache, is.symlink, Sys.readlink)

  if(nrow(symlinks) == 0)
    print("No absolute symlinks found, maybe you ran this script already?")
  else
  {
    #remove absolute links
    unlink(symlinks$linkLocation)
    
    wd       <- getwd()
    on.exit(setwd(wd))
    #padToMax <- generatePadFunction()

    #create the new ones
    for(row in seq(nrow(symlinks)))
    {
      linkLoc <- symlinks[row, "linkLocation"]
      setwd(dirname(linkLoc))
      #print(paste0("For link '", padToMax(symlinks[row, "linkLocation"], 1), "' will convert '", padToMax(symlinks[row, "originalTarget"], 2), "' to '", padToMax(symlinks[row, "linkTarget"], 3), "'"))
      file.symlink(from=symlinks[row, "linkTarget"], to=basename(linkLoc))
      
    }
  }
}

junctionFilename <- function(modulesRoot)
{
  return(pastePath(c(modulesRoot, "..", "junctions.rds")))   
}

collectAndStoreJunctions <- function(buildfolder)
{
  modulesRoot <- pastePath(c(buildfolder, "Modules")) 
  renvCache   <- pastePath(c(buildfolder, "Modules/renv-cache"))
  symlinks    <- collectLinks(modulesRoot, renvCache, is.junction, normalizePath)
  overlap     <- determineOverlap(modulesRoot, modulesRoot)
  relLink     <- lapply(symlinks$linkLocation, overlap$sourceToTarget)
  modules     <- lapply(relLink, function(p) splitPath(p)[[1]])
  links       <- lapply(relLink, function(p) splitPath(p)[[2]])

  if(nrow(symlinks) == 0)
    print("No absolute symlinks found, maybe you ran this script already?")
  else
  {
    juncDF <- data.frame(renv=as.character(symlinks$linkTarget), module=as.character(modules), link=as.character(links))
    saveRDS(juncDF, junctionFilename(modulesRoot))
  }
}

restoreJunctions <- function(modulesRoot)
{
  # Should contain a data.frame with columns: renv, module and link. 
  # As created in collectAndStoreJunctions  
  junctions <- readRDS(junctionFilename(modulesRoot))

  if(nrow(junctions) == 0)
    print("No absolute symlinks found in file, maybe something went wrong building?")
  else
  {
    wd       <- getwd()
    on.exit(setwd(wd))
    padToMax <- generatePadFunction()

    #create the new ones
    for(row in seq(nrow(junctions)))
    {
      renv    <- junctions[row, "renv"  ]
      module  <- junctions[row, "module"]
      link    <- junctions[row, "link"  ]
      modDir  <- pastePath(c(modulesRoot, module))

      if(!file.exists(modDir))
        dir.create(modDir)

      setwd(modDir)
      # print(paste0("Creating junction '", padToMax(link, 1), "' to '", padToMax(renv, 2), "' in '", padToMax(pastePath(c(modulesRoot, module)), 3), "'"))

      if(!dir.exists(link) && link != "BH") #It keeps turning up and this is the easiest way of getting rid of it
      {
        # print(paste0("Creating junction '", padToMax(link, 1), "' with one to '", padToMax(renv, 2), "' in '", padToMax(pastePath(c(modulesRoot, module)), 3), "'"))
        Sys.junction(from=paste0("..\\",renv), to=link)
      }
    }

    cat(NULL, file=paste0(modulesRoot, "\\..\\junctions-recreated-successfully.log"))

  }

}

restoreModulesIfNeeded <- function(jaspFolder)
{
  wd       <- getwd()
  on.exit(setwd(wd))
  setwd(jaspFolder)

  modulesRoot <- pastePath(c(jaspFolder, "Modules"))
  if(!file.exists(modulesRoot))
    dir.create(modulesRoot)
  
  restoreJunctions(pastePath(c(jaspFolder, "Modules")))
}


