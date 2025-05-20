#!/usr/bin/env Rscript 

args = commandArgs(trailingOnly=TRUE)

doTest  <- FALSE
doClone <- FALSE

sapply(args, function(arg) { if(mod == "test")  doTest <- TRUE})
sapply(args, function(arg) { if(mod == "clone") doClone <- TRUE})

modules <- c(
"jaspDescriptives",
"jaspTTests",
"jaspAnova",
"jaspRegression",
"jaspFrequencies",
"jaspFactor",
"jaspAudit",
"jaspBain",
"jaspNetwork",
"jaspMachineLearning",
"jaspMetaAnalysis",
"jaspSem",
"jaspSummaryStatistics",
"jaspMixedModels",
"jaspDistributions",
"jaspEquivalenceTTests",
"jaspJags",
"jaspReliability",
"jaspVisualModeling",
"jaspBFF",
"jaspBfpack",
"jaspEsci"
)

if(doTest) modules <- rbind(modules, "jaspTestModule")


if(doClone) {
    print("Cloning all modules")
    oldwd <- getwd()
    setwd(workdir)

    for(module in modules) {
        system2(paste0("git clone https://github.com/jasp-stats/", module), stdout=TRUE, stderr=TRUE)
    }
    setwd(oldwd)
}

print("Building all modules")
system2(paste0(./buildModuleBundlesLocally.sh ", paste0(workdir, modules)), stdout=TRUE, stderr=TRUE))