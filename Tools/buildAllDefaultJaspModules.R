#!/usr/bin/env Rscript 

args = commandArgs(trailingOnly=TRUE)

# the args thing doesnt seem to work but the defaults are probably what you want anyway. otherwise plerase commit the fix ;)
doTest  = FALSE
doClone = TRUE

sapply(args, (function(arg) { if(arg == "test")  { doTest = TRUE;  }}))
sapply(args, (function(arg) { if(arg == "clone") { doClone = TRUE; }}))

modules = c(
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

if(doTest) modules = rbind(modules, "jaspTestModule")

print('modules:')
print(modules)

workdir     <- './ModuleBundleBuildDir/Modules/'
dir.create(workdir, recursive = TRUE)

if(doClone) {
    print("Cloning all modules")
    oldwd <- getwd()
    setwd(workdir)

    for(module in modules) {
        system2(command="git", args=c("clone", paste0("https://github.com/jasp-stats/", module)))
        system2(command="git", args=c("pull"))
    }
    setwd(oldwd)
}

modulePaths <- paste0(workdir, modules)

print('modulePaths:')
print(modulePaths)

print("Building all modules")
system2(command="./buildModuleBundlesLocally.R", args=modulePaths)

print(paste0("Should you ever want to rebuild the modules from source, you can run: './buildModuleBundlesLocally.R ",workdir,"*' and each module there will be rebuild and placed in '../Modules/local'."))
print("The modules build like this live in your source folder, which means you do not need to rebuild them each time you clean your buildfolder, only when you update R too far or you want to test changes to a module.")
print(paste0("If you just want to rebuild a single module you could just check out the commit you want and then run something like './buildModuleBundlesLocally.R ",workdir,"jaspDescriptives'."))