options(warn=1) #print warnings as they occur
options(nCpus=parallel::detectCores())

CRAN       <- #"https://cran.rstudio.com/"
	"https://cran.r-project.org/" #


#define vars
expEnv          <- NULL
downloaded      <- NULL
pkgDeps         <- NULL
available_pkgs  <- NULL
destDir         <- NULL
destTmpDir      <- NULL
pkgs            <- NULL
base_pkgs       <- NULL
specials        <- NULL

initialize <- function()
{
  #Create necessary environments (only hash available)
  expEnv     <<- new.env(hash = TRUE, parent = parent.frame())
  downloaded <<- new.env(hash = TRUE, parent = parent.frame())
  pkgDeps    <<- new.env(hash = TRUE, parent = parent.frame())

  #what packages are available? Do we care?
  available_pkgs  <<- available.packages(repos = CRAN)
  destDir         <<- "pkg-source-files/"
  destTmpDir      <<- "pkg-source-tmp-files/" 

  # Define the base packages.  These are packages which come with R upon install
  # of R.  These packages include: "base", "compiler", "datasets", "graphics",
  # "grDevices", "grid", "methods", "parallel", "splines", "stats", "stats4",
  # "tcltk", "tools", and "utils".
  #
  # NOTE: there are Priority = "recommended" packages as well.  If these packages
  # are missing from the system install, this script might fail.  Downloading and
  # installing the 'recommended' packages can be difficult between R versions.
  base_pkgs      <<- unname(utils::installed.packages()[utils::installed.packages()[, "Priority"] %in% c("base", "recommended"), "Package"])

  cat('get the packages that JASP expects and put them in an env that includes a specific version\n')
  source("../../JASP-Engine/jasp-r-pkg/R/packagecheck.R", local=TRUE)
  expected   <- .expectedPackages()
  pkgs       <<- expected[,1]

  for (curPkg in pkgs)
  {
    expVersion        <- expected[curPkg, "Version"]
    expEnv[[curPkg]]  <- expVersion
  }

  if(!dir.exists(destDir))
    dir.create(destDir)

  if(!dir.exists(destTmpDir))
    dir.create(destTmpDir)
}


defineSpecials <- function()
{
  specials <<- new.env(hash = TRUE, parent = parent.frame())
  specials[['bstats']]       <- list(type='github', commit='1b0b925d0404537b908a6380b70d80382df2d374', repo='AlexanderLyNL/bstats'  )
  specials[['flexplot']]     <- list(type='github', commit='163137fe30d9541234eb0053d6d86e1f96cd3dc5', repo='dustinfife/flexplot'   )
  specials[['Bayesrel']]     <- list(type='github', commit='8bd009e3360fe8ae8db36bb1b1f640c51ca92e97', repo='juliuspf/Bayesrel'     )
  specials[['stanova']]      <- list(type='github', commit='3e5635816fb2e4cda06704778e5bcd382f14717d', repo='bayesstuff/stanova'    )
  #specials[['afex']]         <- list(type='github', commit='71e22f0020399de1b35189d7c0dd4e5a2729b843', repo='singmann/afex'         )
  specials[['ggpol']]        <- list(type='github', commit='dea9db2503b04b81dbc746fdeccf92e9849ce64b', repo='jasp-stats/ggpol'      ) # temporary fix for conflicting ggplot2 dependencies in jasp 0.12.2. Should be removed after that release and shit should be fixed!
  #specials[['RoBMA']]        <- list(type='github', commit='7685d4d203a46f259b4cda2c492c8235397c80eb', repo='FBartos/RoBMA'         )
  
}

getPkgVersion <- function(pkgName)
{
  if(!exists(pkgName, where=expEnv, inherits=FALSE))
  {
    # so we do not know what version we need... Because this pkg was unexpected
    cat(paste0("Checking version of pkg ", pkgName, " in available_pkgs\n"))
    expEnv[[pkgName]] <- available_pkgs[[pkgName, "Version"]]
    cat(paste0("Couldn't find version of ",pkgName," in expected packages so took it from availablePackages: ", expEnv[[pkgName]], '\n')) 
  }

  return(expEnv[[pkgName]])
}

downloadPkgGithub <- function(pkgName)
{
  # example https://github.com/quentingronau/abtest/tarball/503c50e96768a0134b755747e0421d820cc1a115
  repo        <- specials[[pkgName]]$repo
  commit      <- specials[[pkgName]]$commit
  succes      <- FALSE
  tarballUrl  <- paste0('https://github.com/', repo, '/tarball/', commit)
  destFile    <- paste0(destTmpDir, '/', pkgName, '_', commit, '.tar.gz')
  
  if(!file.exists(destFile))
  {
    cat(paste0('For pkg ', pkgName, ' getting github tarball download from "',tarballUrl,'" to: "', destFile, '"', '\n'))

    tryCatch(error=function(e) e, exp=
    {
      download.file(url=tarballUrl, destfile=destFile);
      succes <- TRUE;
    })

    if(!succes)
      stop("Failed to get pkg sources for '", pkgName, "' from github for repo: '", repo, "' and commit '", commit, "'")
  }

  return(destFile)
}

downloadPkgCran <- function(curPkg)
{
  version <- getPkgVersion(curPkg)
  filePkg  <- paste0(curPkg, "_", expEnv[[curPkg]],".tar.gz")
  destFile <- paste0(destDir, filePkg)

  downloaded[[curPkg]] <- list(version=version, fileName=filePkg, destFile=destFile, downloadUrl=paste0("http://static.jasp-stats.org/RPkgs/", filePkg), sha256='')
  
  if(!file.exists(destFile))
  {  
    cat(paste0("File ",destFile, " does not exist yet, try to get it from CRAN.\n"))
    curVer   <- paste0(CRAN, 'src/contrib/', filePkg)
    oldVer   <- paste0(CRAN, 'src/contrib/Archive/', curPkg, '/', filePkg)
    succes   <- FALSE

    cat(paste0('For pkg ', curPkg, ' trying download from "',curVer,'"\n'))

    tryCatch(error=function(e) e, exp=
    {
      download.file(url=curVer, destfile=destFile);
      succes <- TRUE;
    })
    
    if(!succes)
    {
      cat(paste0('Download failed, now trying: "',oldVer,'"\n'))  
      tryCatch(error=function(e) e, exp={
        download.file(url=oldVer, destfile=destFile);
        succes <- TRUE;
      })
    }

    if(!succes)
      stop("Both downloads failed...")
  }
 
  sha256sumOutput                  <- system2('sha256sum', args=destFile, stdout=TRUE)
  downloaded[[curPkg]]$sha256      <- strsplit(sha256sumOutput, ' ')[[1]][[1]]

  return(destFile)

}


downloadPkg <- function(pkgName)
{
  if(is.null(specials)) defineSpecials()

  filename <- "???"

  #check if special case then go to github
  filename <- ifelse(
    exists(pkgName, where=specials, inherits=FALSE), 
    yes = downloadPkgGithub(pkgName), 
    no  = downloadPkgCran(pkgName)
  )

  #github doesnt need to check if file downloaded because it is just a few
  #otherwise try to get it from Cran if it isnt in pkg-sources yet

  return(filename)
}

getDeps <- function(pkgName)
{
  if(pkgName %in% base_pkgs)
    return(c())

  if(exists(pkgName, where=pkgDeps, inherits=FALSE)) #Avoid double work because time is precious
  {
    cat(paste0('Dependencies for pkg "', pkgName, '" requested more than once...\n'))
    return(pkgDeps[[pkgName]]) #in case someone wants em?
  }

  #make sure the source pkg is in the pkg-source folder:
  pkgFile <- downloadPkg(pkgName)

  deps    <- remotes::dev_package_deps(pkgFile, repos = NULL, type = "source")$package
  deps    <- deps[!(deps %in% base_pkgs)] #remove base pkgs

  pkgDeps[[pkgName]] <- deps

  padding <- strrep(" ", 29 - nchar(pkgName))
  depstr  <- paste(deps, sep=', ', collapse=', ')

  cat(paste0("Dependencies for pkg '", pkgName, "': ", padding, "'", depstr, "'\n"))

  return(deps)
}


giveOrderedDependencies <- function(addDeps=TRUE)
{
	initialize()
  cat("Determing dependencies and their order\n")

  i <- 1L
  while(i <= length(pkgs)) 
  {
    curPkg  <- pkgs[i]
    deps    <- getDeps(curPkg)

    if(addDeps) 
      pkgs <- append(pkgs, deps[!(deps %in% pkgs)])
    
    i <- i + 1L
  }

  cat('Now we must make sure that all pkgs are preceded by their dependencies and they by theirs\n')
  orderedPkgs   <- character()
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
          orderedPkgs       <- c(orderedPkgs, curPkg)
          remove(list=curPkg, envir=pkgDeps, inherits=FALSE) # We remove it from the pkgDeps env to make sure we know we finish eventually
          #cat(paste0('Pkg ',curPkg,' depends on #', length(deps),' of deps {' ,paste0(deps, collapse='', sep=', '), '} and was added to ordered list\n'))
        }
      }
      i <- i + 1L
    }

    pkgs_to_order  <- sort(names(pkgDeps))
    cat(paste0('pkgs_to_order was rebuilt from pkgDeps and now has length: ', length(pkgs_to_order), '\n'))

    if(lastTime == length(pkgs_to_order))
    {
      cat('pkgDeps is not getting smaller anymore... Currently it consists of: "', paste(pkgs_to_order, sep=", ", collapse=", "), '"\n')
      stop('list isnt decreasing in size anymore... bailing!')
    }

    lastTime <- length(pkgs_to_order)
  }

  orderedPkgs <- orderedPkgs[!(orderedPkgs %in% base_pkgs)] #remove base pkgs, because somehow they sneaked back in!
  orderedPkgs <- orderedPkgs[!(orderedPkgs %in% c("graph"))]
  #cat(paste0('Pkgs ordered by dependencies: ', paste0(orderedPkgs, sep=', ', collapse='')))

  return(orderedPkgs);
}

#Dont forget to add the dependencies up top
generateRCmdInstall <- function(configargs="")
{
  return(
    paste0(
      ind,'\t"build-commands": [ "R CMD INSTALL .', ifelse(configargs == "", "", paste0(" --configure-args=",configargs)) ,'" ]\n',ind,'}',
      sep='',
      collapse=''
    )
  );
}

ind <- '\t\t'

convertToJsonGitBuild <- function(pkgName)
{
  repo   <- paste0('https://github.com/', specials[[pkgName]]$repo)
  commit <- specials[[pkgName]]$commit

  return(paste0(
    ind,'{\n',ind,'\t"name": "', 
    pkgName, 
    '",\n',ind,'\t"buildsystem": "simple",\n',ind,'\t"sources": [\n',ind,'\t\t{\n',ind,'\t\t\t"type": "git",\n',ind,'\t\t\t"url": "', 
    repo, 
    '",\n',ind,'\t\t\t"commit": "', 
    commit,
    '"\n',ind,'\t\t}\n',ind,'\t],\n',
    generateRCmdInstall(),
    sep='',
    collapse=''))
}

convertToJsonDefault <- function(pkgName)
{
  needsJAGSInfo = (pkgName == "runjags"); #rjags is smart enough, but runjags isnt...

  pkgUrl <- downloaded[[pkgName]]$downloadUrl
  pkgSha <- downloaded[[pkgName]]$sha256

  return(
    paste0(
      ind,'{\n',ind,'\t"name": "', 
      pkgName, 
      '",\n',ind,'\t"buildsystem": "simple",\n',ind,'\t"sources": [\n',ind,'\t\t{\n',ind,'\t\t\t"type": "archive",\n',ind,'\t\t\t"url": "', 
      pkgUrl, 
      '",\n',ind,'\t\t\t"sha256": "', 
      pkgSha,
      '"\n',ind,'\t\t}\n',ind,'\t],\n',
      generateRCmdInstall(ifelse(needsJAGSInfo, "'--with-jags-include=/app/include/JAGS --with-jags-lib=/app/lib/'" , '')),
      sep='',
      collapse=''
    )
  )
}

convertToJsonEntry <- function(pkgName)
{
  # check if something special should be done for this package
  if(!exists(pkgName, where=specials, inherits=FALSE))
    return(convertToJsonDefault(pkgName))
  
  switch(specials[[pkgName]]$type,
    github = return(convertToJsonGitBuild(pkgName))
  )
  
  stop(paste0("Found a special for pkg '", pkgName, "' that I cannot handle because type is: '", specials[[pkgName]]$type, "'")) 
}

createFlatpakJson <- function()
{
  orderedPkgs <- giveOrderedDependencies()

  jsonLines <- c('{\n\t"name": "RPackages",\n\t"buildsystem": "simple",\n\t"build-commands": [],\n\t"modules":\n\t[', paste0(as.character(lapply(orderedPkgs, convertToJsonEntry)), collapse=",\n"), '\n\t]\n}\n')
  jsonFile  <- "RPackages.json"
  
  fileConn  <- file(jsonFile)
  writeLines(jsonLines, fileConn)
  close(fileConn)

  cat(paste0("Expected packages are written as json to ", jsonFile, " and org.jaspstats.JASP.json knows where to look for it.\n"))
}

getInstalledPackageEnv <- function()
{
  installed           <- as.data.frame(installed.packages(lib.loc = .libPaths()[1])[,c(1,3:4)]);
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

installRequiredPackages <- function(stopOnError = TRUE)
{
  orderedPkgs   <- giveOrderedDependencies()
  installedPkgs <- getInstalledPackageEnv()
  errorPkgs     <- c()
  
  
  error <- NULL
  for (pkgName in orderedPkgs)
  {

    version  <- expEnv[[pkgName]]
    isThere  <- exists(pkgName, where=installedPkgs, inherits=FALSE)
    
    if(isThere)
      isThere <- (installedPkgs[[pkgName]] == version)

    cat(paste0('Getting ready to install ',pkgName,' ',version, '\n'))
    
    if(isThere)
      cat('Already installed!\n')
    else if(exists(pkgName, where=specials, inherits=FALSE))
    {
      specialDef <- specials[[pkgName]]
      if(specialDef$type == 'github')
        error <- try(devtools::install_github(paste0(specialDef$repo, '@', specialDef$commit)))
      else
        stop(paste0("Found a special that I cannot handle! (",specialDef,")"))
    }
    else
      error <- try(devtools::install_version(package=pkgName, version=version, repos=CRAN, dependencies=FALSE, upgrade_dependencies=FALSE))

    if (inherits(error, "try-error"))
    {
      if(stopOnError) stop(error)
      else            errorPkgs <- c(errorPkgs, pkgName)
    }
      
  }


  cat('All packages installed!\n')

  if(!stopOnError && length(errorPkgs) > 0)
    cat("Installing the following pkgs failed: ", paste(errorPkgs, sep=", ", collapse=", "), "\n")
}

cat('Run createFlatpakJson() to transform the expected packages of JASP into a fresh org.jaspstats.JASP.json for flatpak.\nOr run installRequiredPackages() as administrator to get you local installed version of R up to speed with the same packages.\n')
