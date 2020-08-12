.libPaths("~/pkgs/Frameworks/R.framework/Versions/3.6/Resources/library")

repos <- getOption("repos")
repos["CRAN"] <- "https://cloud.r-project.org"
options(repos = repos)
options(pkgType="binary")

install.packages("remotes")

jaspResultsPath <- file.path("..", "..", "..", "JASP-R-Interface", "jaspResults")
remotes::install_deps(jaspResultsPath, upgrade=FALSE)
install.packages(jaspResultsPath, type="source", repos=NULL)

jaspGraphsPath <- file.path("..", "..", "..", "JASP-Engine", "jaspGraphs")
remotes::install_deps(jaspGraphsPath, upgrade=FALSE)
install.packages(jaspGraphsPath, type="source", repos=NULL)

JASPPath <- file.path("..", "..", "..", "JASP-Engine", "jaspBase")
# remotes::install_deps(JASPPath, upgrade=FALSE) # JASP doesn't have any dependencies (yet)
install.packages(JASPPath, type="source", repos=NULL)

jasptoolsPath <- file.path("..", "..", "..", "Tools", "jaspTools")
remotes::install_deps(jasptoolsPath, upgrade=FALSE)
install.packages(jasptoolsPath, type="source", repos=NULL)
