.libPaths("~/pkgs/Frameworks/R.framework/Versions/3.5/Resources/library")

repos <- getOption("repos")
repos["CRAN"] <- "https://cloud.r-project.org"
options(repos = repos)
options(pkgType="binary")

install.packages("devtools")

jaspResultsPath <- file.path("..", "..", "..", "JASP-R-Interface", "jaspResults")
devtools::install_deps(jaspResultsPath, upgrade=FALSE)
install.packages(jaspResultsPath, type="source", repos=NULL)

jasptoolsPath <- file.path("..", "..", "..", "Tools", "jasptools")
devtools::install_deps(jasptoolsPath, upgrade=FALSE)
install.packages(jasptoolsPath, type="source", repos=NULL)

JASPgraphsPath <- file.path("..", "..", "..", "JASP-Engine", "JASPgraphs")
devtools::install_deps(JASPgraphsPath, upgrade=FALSE)
install.packages(JASPgraphsPath, type="source", repos=NULL)