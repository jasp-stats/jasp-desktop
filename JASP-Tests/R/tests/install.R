.libPaths("~/pkgs/Frameworks/R.framework/Versions/3.6/Resources/library")

repos <- getOption("repos")
repos["CRAN"] <- "https://cloud.r-project.org"
options(repos = repos)
options(pkgType="binary")

install.packages("remotes")

jaspResultsPath <- file.path("..", "..", "..", "JASP-R-Interface", "jaspResults")
remotes::install_deps(jaspResultsPath, upgrade=FALSE)
install.packages(jaspResultsPath, type="source", repos=NULL)

JASPgraphsPath <- file.path("..", "..", "..", "JASP-Engine", "JASPgraphs")
remotes::install_deps(JASPgraphsPath, upgrade=FALSE)
install.packages(JASPgraphsPath, type="source", repos=NULL)

JASPPath <- file.path("..", "..", "..", "JASP-Engine", "JASP")
# remotes::install_deps(JASPPath, upgrade=FALSE) # JASP doesn't have any dependencies (yet)
install.packages(JASPPath, type="source", repos=NULL)

jasptoolsPath <- file.path("..", "..", "..", "Tools", "jasptools")
remotes::install_deps(jasptoolsPath, upgrade=FALSE)
install.packages(jasptoolsPath, type="source", repos=NULL)
