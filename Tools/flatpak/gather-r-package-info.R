# very loosely based on https://github.com/dewittpe/R-install-dependencies
options(warn=1) #print warnings as they occur
options(nCpus=8)

expEnv     <- new.env(hash = TRUE, parent = parent.frame())
CRAN       <- "https://cran.r-project.org/" #"https://cran.rstudio.com/"

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
  #print(paste0('Pkgs ordered by dependencies: ', paste0(orderedPkgs, sep=', ', collapse='')))

  print('Now make sure also the versions of those packages that were included as a dependency have a version specified in expEnv')
  i <- 1L
  while(i <= length(orderedPkgs))
  {
    curPkg  <- orderedPkgs[[i]]

    if(!exists(curPkg, where=expEnv, inherits=FALSE)) #if so get the version from available pkgs
    {
      expEnv[[curPkg]] <- available_pkgs[[curPkg, "Version"]]
      print(paste0("Couldn't find version of ",curPkg," in expected packages so took it from availablePackages: ", expEnv[[curPkg]]))    
    }

    i <- i + 1
  }  

  return(orderedPkgs);
}

specials <- new.env(hash = TRUE, parent = parent.frame())
specials[['BAS']]   <- list(type='github', commit='abb73a6ac9d145ced3586434d413130b2f6263e9', repo='vandenman/BAS')
specials[['Bain']]  <- list(type='github', commit='1b03f71204839da29a4219e8bba99b8ec8479612', repo='jasp-stats/BAIN-for-JASP')

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
        downloaded[[curPkg]]$downloadUrl <- curVer
      })
      
      if(!succes)
      {
        print(paste0('Download failed, now trying: "',oldVer,'"'))  
        tryCatch(error=function(e) e, exp={
          download.file(url=oldVer, destfile=destFile);
          succes <- TRUE;
          downloaded[[curPkg]]$downloadUrl <- oldVer
        })
      }

      if(!succes)
        stop("Both downloads failed...")

      sha256sumOutput             <- system2('sha256sum', args=destFile, stdout=TRUE)
      downloaded[[curPkg]]$sha256 <- strsplit(sha256sumOutput, ' ')[[1]][[1]]
    }

    i <- i + 1L
  }

  ind             <- '\t\t'
  buildOptionsEtc <- paste0(
    ind,'\t"build-options": {\n',ind,'\t\t"append-ld-library-path": "/app/lib;/app/lib64/R/lib",\n',
    ind,'\t\t"env": {\n',ind,'\t\t\t"GIT_DISCOVERY_ACROSS_FILESYSTEM": "true",\n',ind,'\t\t\t"R_HOME": "/app/lib64/R/",\n',ind,'\t\t\t"PREFIX": "/app"\n',ind,'\t\t}\n',ind,'\t},\n',
    ind,'\t"build-commands": [ "R CMD INSTALL ." ]\n',ind,'},\n',
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

  jsonLines <- as.character(lapply(orderedPkgs, convertToJsonLine))
  #print(jsonLines)
  jsonFile  <- "expectedPackages.json"
  fileConn  <- file(jsonFile)
  writeLines(jsonLines, fileConn)
  close(fileConn)


  system2("cat",args=c("org.jasp-stats.JASP_header.json", jsonFile, "org.jasp-stats.JASP_footer.json"), stdout="org.jasp-stats.JASP.json")

  print(paste0("Expected packages are written as json to ", jsonFile, " and a fresh org.jasp-stats.JASP.json has been generated!"))
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

print('Run createFlatpakJson() to transform the expected packages of JASP into a fresh org.jasp-stats.JASP.json for flatpak.\nOr run installRequiredPackages() as administrator to get you local installed version of R up to speed with the same packages.')
