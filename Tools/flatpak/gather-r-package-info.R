# very loosely based on https://github.com/dewittpe/R-install-dependencies
options(warn=1) #print warnings as they occur
options(nCpus=8)



expEnv     <- new.env(hash = TRUE, parent = parent.frame())
CRAN       <- #"https://cran.rstudio.com/"
	"https://cran.r-project.org/" #

#At least one package (graph) moved to Bioconductor, so should be using BiocManager because it supports both CRAN and Bioconductor packages?
#install.packages("BiocManager", repos=CRAN)
# this needs a bit of a rewrite though... so I will first try to do a quick manual fix and hope nothing breaks
  

giveOrderedDependencies <- function()
{
  available_pkgs  <- available.packages(repos = CRAN)

  # Define the base packages.  These are packages which come with R upon install
  # of R.  These packages include: "base", "compiler", "datasets", "graphics",
  # "grDevices", "grid", "methods", "parallel", "splines", "stats", "stats4",
  # "tcltk", "tools", and "utils".
  #
  # NOTE: there are Priority = "recommended" packages as well.  If these packages
  # are missing from the system install, this script might fail.  Downloading and
  # installing the 'recommended' packages can be difficult between R versions.
  base_pkgs      <- unname(utils::installed.packages()[utils::installed.packages()[, "Priority"] %in% c("base", "recommended"), "Package"])

  print('get the packages that JASP expects and put them in an env that includes a specific version')
  source("../../JASP-Engine/JASP/R/packagecheck.R", local=TRUE)
  expected   <- .expectedPackages()
  pkgs       <- expected[,1]

  i          <- 1L
  while(i <= length(pkgs))
  {
    curPkg            <- pkgs[[i]]
    expVersion        <- expected[curPkg, "Version"]
    expEnv[[curPkg]]  <- expVersion
    i                 <- i + 1L
  }

  # use the tools::package_dependencies function to generate an environment with pkg->deps
  # the environment is only used because it is apparently the sole implementation of a hashmap in R

  i                 <- 1L
  pkgDeps           <- new.env(hash = TRUE, parent = parent.frame(), size = length(pkgs))

  while(i <= length(pkgs)) 
  {
    curPkg  <- pkgs[i]

    if(!exists(curPkg, where=pkgDeps, inherits=FALSE)) #Avoid double work because time is precious
    {
      deps    <-
        unlist(tools::package_dependencies( packages  = curPkg,
                                            which     = c("Depends", "Imports", "LinkingTo"),
                                            db        = available_pkgs,
                                            recursive = FALSE),
                                            use.names = FALSE)

      deps              <- deps[!(deps %in% base_pkgs)] #remove base pkgs

      #if(curPkg == "KneeArrower") deps <- append(deps, "signal") #workaround... Becacuse KneeArrower is taken from github apparently the dependencies aren't taken into account properly.
      if(curPkg == "bstats")       deps <- append(deps, c("hypergeo", "purrr", "SuppDists"))
      if(curPkg == "flexplot")     deps <- append(deps, c("ggplot2", "cowplot", "tibble", "withr", "dplyr", "magrittr", "forcats", "purrr", "plyr", "R6"))

      pkgDeps[[curPkg]] <- deps
      pkgs              <- append(pkgs, deps, i)
    }
    
    i <- i + 1L
  }

  print('Now we must make sure that all pkgs are preceded by their dependencies and they by theirs')
  orderedPkgs   <- list()
  inList        <- new.env(hash = TRUE, parent = parent.frame(), size = length(pkgs))
  lastTime      <- -1
  pkgs_to_order <- sort(names(pkgDeps))

  while(length(pkgs_to_order) > 0)
  {
    i <- 1L

    while(i <= length(pkgs_to_order))
    {
      curPkg <- pkgs_to_order[i]
      deps   <- pkgDeps[[curPkg]]

      if(!exists(curPkg, where=inList, inherits=FALSE))
      {
        insert <- length(deps) == 0
        
        if(!insert) #maybe all dependencies are alread in list
        {
          insert <- TRUE

          for(dep in deps)
            if(!exists(dep, where=inList, inherits=FALSE))
              insert <- FALSE
        }

        if(insert)
        {
          inList[[curPkg]]  <- deps # no point in losing the deps even though we don't really need them after this
          orderedPkgs       <- append(orderedPkgs, curPkg)
          remove(list=curPkg, envir=pkgDeps, inherits=FALSE)
          #print(paste0('Pkg ',curPkg,' depends on #', length(deps),' of deps {' ,paste0(deps, collapse='', sep=', '), '} and was added to ordered list'))
        }
      }
      i <- i + 1L
    }

    pkgs_to_order  <- sort(names(pkgDeps))
    #print(paste0('pkgs_to_order was rebuilt from pkgDeps and now has length: ', length(pkgs_to_order)))

    if(lastTime == length(pkgs_to_order))
      stop('list isnt decreasing in size anymore... bailing!')

    lastTime <- length(pkgs_to_order)
  }

  orderedPkgs <- orderedPkgs[!(orderedPkgs %in% base_pkgs)] #remove base pkgs, because somehow they sneaked back in!
  orderedPkgs <- orderedPkgs[!(orderedPkgs %in% c("graph"))]
  #print(paste0('Pkgs ordered by dependencies: ', paste0(orderedPkgs, sep=', ', collapse='')))

  print('Now make sure also the versions of those packages that were included as a dependency have a version specified in expEnv')
  i <- 1L
  while(i <= length(orderedPkgs))
  {
    curPkg  <- orderedPkgs[[i]]

    if(!exists(curPkg, where=expEnv, inherits=FALSE)) #if so get the version from available pkgs
    {
	  print(paste0("Checking version of pkg ", curPkg, " in available_pkgs"))
      expEnv[[curPkg]] <- available_pkgs[[curPkg, "Version"]]
      print(paste0("Couldn't find version of ",curPkg," in expected packages so took it from availablePackages: ", expEnv[[curPkg]]))    
    }

    i <- i + 1
  }  

  return(orderedPkgs);
}

#Dont forget to add the dependencies up top
specials <- new.env(hash = TRUE, parent = parent.frame())
specials[['abtest']]       <- list(type='github', commit='503c50e96768a0134b755747e0421d820cc1a115', repo='quentingronau/abtest')
#specials[['BAS']]          <- list(type='github', commit='daba70f5a5d60bfa63386d4e6a6522f86a04946c', repo='merliseclyde/BAS')
specials[['bstats']]       <- list(type='github', commit='a6fdbea42078b8d275a98dd1e37c113118555b6f', repo='AlexanderLyNL/bstats')
#specials[['Bain']]         <- list(type='github', commit='1b03f71204839da29a4219e8bba99b8ec8479612', repo='jasp-stats/BAIN-for-JASP')
#specials[['KneeArrower']]  <- list(type='github', commit='cdb14e574e00914e4e7019a4cf3c5fcda7426466', repo='agentlans/KneeArrower')
specials[['flexplot']]     <- list(type='github', commit='46adae504c83b6dbd7baf7ba679cc95d0fb5c1a9', repo='dustinfife/flexplot')


createFlatpakJson <- function()
{
  orderedPkgs <- giveOrderedDependencies()

  #download the specified versions of the pkgs and remember relevant stuff
  downloaded <- new.env(hash = TRUE, parent = parent.frame(), size = length(orderedPkgs))
  destDir <- "pkg-source-files/"

  unlink(paste0(destDir,"*"))
  dir.create(destDir)

  i <- 1L
  while(i <= length(orderedPkgs))
  {
    curPkg   <- orderedPkgs[[i]]

    if(!exists(curPkg, where=specials, inherits=FALSE))
    {
      version  <- expEnv[[curPkg]]
      filePkg  <- paste0(curPkg, "_", expEnv[[curPkg]],".tar.gz")
      destFile <- paste0(destDir, filePkg)
      curVer   <- paste0(CRAN, 'src/contrib/', filePkg)
      oldVer   <- paste0(CRAN, 'src/contrib/Archive/', curPkg, '/', filePkg)
      succes   <- FALSE

      downloaded[[curPkg]] <- list(version=version, fileName=filePkg, destFile=destFile, downloadUrl='', sha256='')

      print(paste0('For pkg ', curPkg, ' trying download from "',curVer,'"'))
      tryCatch(error=function(e) e, exp={
        download.file(url=curVer, destfile=destFile);
        succes <- TRUE;
        #downloaded[[curPkg]]$downloadUrl <- curVer
      })
      
      if(!succes)
      {
        print(paste0('Download failed, now trying: "',oldVer,'"'))  
        tryCatch(error=function(e) e, exp={
          download.file(url=oldVer, destfile=destFile);
          succes <- TRUE;
          #downloaded[[curPkg]]$downloadUrl <- oldVer
        })
      }

      if(!succes)
        stop("Both downloads failed...")

      sha256sumOutput                  <- system2('sha256sum', args=destFile, stdout=TRUE)
      downloaded[[curPkg]]$sha256      <- strsplit(sha256sumOutput, ' ')[[1]][[1]]
      downloaded[[curPkg]]$downloadUrl <- paste0("http://static.jasp-stats.org/RPkgs/", filePkg)
    }

    i <- i + 1L
  }

  ind             <- '\t\t'
  buildOptionsEtc <- paste0(
    ind,'\t"build-commands": [ "R CMD INSTALL ." ]\n',ind,'}',
    sep='',
    collapse='')

  convertToJsonGitBuild <- function(pkgName, repo, commit)
  {
    return(paste0(
      ind,'{\n',ind,'\t"name": "', 
      pkgName, 
      '",\n',ind,'\t"buildsystem": "simple",\n',ind,'\t"sources": [\n',ind,'\t\t{\n',ind,'\t\t\t"type": "git",\n',ind,'\t\t\t"url": "', 
      repo, 
      '",\n',ind,'\t\t\t"commit": "', 
      commit,
      '"\n',ind,'\t\t}\n',ind,'\t],\n',
      buildOptionsEtc,
      sep='',
      collapse=''))
  }

  

  # Now we just need to convert this to a json to use in flatpak

  convertToJsonLine <- function(pkgName)
  {
    # check if something special should be done for this package
    if(exists(pkgName, where=specials, inherits=FALSE))
    {
      specialDef <- specials[[pkgName]]
      if(specialDef$type == 'github')
        return(convertToJsonGitBuild(pkgName, paste0('https://github.com/',specialDef$repo), specialDef$commit))
      else
        stop(paste0("Found a special that I cannot handle because type is: ", specialDef$type))
    }

    pkgUrl <- downloaded[[pkgName]]$downloadUrl
    pkgSha <- downloaded[[pkgName]]$sha256
    return(paste0(
      ind,'{\n',ind,'\t"name": "', 
      pkgName, 
      '",\n',ind,'\t"buildsystem": "simple",\n',ind,'\t"sources": [\n',ind,'\t\t{\n',ind,'\t\t\t"type": "archive",\n',ind,'\t\t\t"url": "', 
      pkgUrl, 
      '",\n',ind,'\t\t\t"sha256": "', 
      pkgSha,
      '"\n',ind,'\t\t}\n',ind,'\t],\n',
      buildOptionsEtc,
      sep='',
      collapse=''))
  }

  jsonLines <- c('{\n\t"name": "RPackages",\n\t"buildsystem": "simple",\n\t"build-commands": [],\n\t"modules":\n\t[', paste0(as.character(lapply(orderedPkgs, convertToJsonLine)), collapse=",\n"), '\n\t]\n}\n')
  #print(jsonLines)
  jsonFile  <- "RPackages.json"
  fileConn  <- file(jsonFile)
  writeLines(jsonLines, fileConn)
  close(fileConn)


  #system2("cat",args=c("org.jaspstats.JASP_header.json", jsonFile, "org.jaspstats.JASP_footer.json"), stdout="org.jaspstats.JASP.json")

  print(paste0("Expected packages are written as json to ", jsonFile, " and org.jaspstats.JASP.json knows where to look for it."))
}

getInstalledPackageEnv <- function()
{
  installed           <- as.data.frame(installed.packages()[,c(1,3:4)]);
  rownames(installed) <- NULL
  installed           <- installed[is.na(installed$Priority),1:2,drop=FALSE]
  installEnv          <- new.env(hash = TRUE, parent = parent.frame(), size=length(installed))
    
  i <- 1L
  while(i <= nrow(installed))
  {
    pkg               <- as.character(installed$Package[[i]])
    ver               <- as.character(installed$Version[[i]])
    installEnv[[pkg]] <- ver
    i                 <- i + 1L
  }
  
  return(installEnv)
}

installRequiredPackages <- function()
{
  orderedPkgs   <- giveOrderedDependencies()
  installedPkgs <- getInstalledPackageEnv()
  
  
  
  i <- 1L
  while(i <= length(orderedPkgs))
  {
    pkgName  <- orderedPkgs[[i]]
    version  <- expEnv[[pkgName]]
    isThere  <- exists(pkgName, where=installedPkgs, inherits=FALSE)
    
    if(isThere)
      isThere <- (installedPkgs[[pkgName]] == version)

    print(paste0('Getting ready to install ',pkgName,' ',version))
    
    if(isThere)
      print('Already installed!')
    else if(exists(pkgName, where=specials, inherits=FALSE))
    {
      specialDef <- specials[[pkgName]]
      if(specialDef$type == 'github')
        devtools::install_github(paste0(specialDef$repo, '@', specialDef$commit))
      else
        stop(paste0("Found a special that I cannot handle! (",specialDef,")"))
    }
    else
      devtools::install_version(package=pkgName, version=version, repos=CRAN, dependencies=FALSE, upgrade_dependencies=FALSE)  
      
    i <- i + 1L
  }


  print('All packages installed!')
}

print('Run createFlatpakJson() to transform the expected packages of JASP into a fresh org.jaspstats.JASP.json for flatpak.\nOr run installRequiredPackages() as administrator to get you local installed version of R up to speed with the same packages.')
