QT -= core gui

exists(/app/lib/*){
	message("On flatpak!")
	JASP_BUILDROOT_DIR	= /app/bin
	mkpath($${JASP_BUILDROOT_DIR}/Modules)

	MODULES_RENV_ROOT	= /app/lib64/renv-root
	MODULES_RENV_CACHE	= /app/lib64/renv-cache
	R_REPOSITORY		= file:///app/lib64/local-cran

}else{

	JASP_BUILDROOT_DIR  = $$OUT_PWD/.. #We expect this to be run from at least one folder deep into the buildroot! Engine/Desktop/Etc

	MODULES_RENV_ROOT   = $$SYS_RENV_ROOT
	isEmpty(MODULES_RENV_ROOT):		MODULES_RENV_ROOT	= "$$OUT_PWD/../renv-root"  #I assume we will not need to ship this, but we do need the following folder:
									MODULES_RENV_CACHE	= "$$OUT_PWD/../renv-cache" #While this one needs to be shipped and the symlinks will need to be relative, but we will find out quick enough if not

	mkpath($$MODULES_RENV_ROOT)
	mkpath($$MODULES_RENV_CACHE)

	R_REPOSITORY = $$JASP_R_REPOSITORY
	isEmpty(R_REPOSITORY): R_REPOSITORY = "https://cloud.r-project.org/"

}

message(Modules uses JASP_BUILDROOT_DIR of $${JASP_BUILDROOT_DIR})
message("Using renv-root: $$MODULES_RENV_ROOT")
message("Using renv-cache: $$MODULES_RENV_CACHE")
message("Using repository: $$R_REPOSITORY")

SUPPORTED_LANGUAGES = nl de es #Just setting this once is enough...

include(../JASP.pri)
include(../R_HOME.pri)

TEMPLATE = aux
CONFIG -= app_bundle

MODULE_DIR			= $$PWD

################################## Common ##################################
PREVIOUS_MODULE = 
MODULE_NAME = jaspDescriptives
include(InstallModule.pri)

MODULE_NAME = jaspAnova
include(InstallModule.pri)

MODULE_NAME = jaspFactor
include(InstallModule.pri)

MODULE_NAME = jaspFrequencies
include(InstallModule.pri)

MODULE_NAME = jaspRegression
include(InstallModule.pri)

MODULE_NAME = jaspTTests
include(InstallModule.pri)

MODULE_NAME = jaspMixedModels
include(InstallModule.pri)

################################## Extra ##################################
MODULE_NAME = jaspAudit
include(InstallModule.pri)

MODULE_NAME = jaspBain
include(InstallModule.pri)

MODULE_NAME = jaspNetwork
include(InstallModule.pri)

MODULE_NAME = jaspSem
include(InstallModule.pri)

MODULE_NAME = jaspMachineLearning
include(InstallModule.pri)

MODULE_NAME = jaspSummaryStatistics
include(InstallModule.pri)

MODULE_NAME = jaspMetaAnalysis
include(InstallModule.pri)

MODULE_NAME = jaspDistributions
include(InstallModule.pri)

MODULE_NAME = jaspEquivalenceTTests
include(InstallModule.pri)

MODULE_NAME = jaspJags
include(InstallModule.pri)

MODULE_NAME = jaspReliability
include(InstallModule.pri)

#MODULE_NAME = jaspVisualModeling
#include(InstallModule.pri)

MODULE_NAME = jaspLearnBayes
include(InstallModule.pri)

MODULE_NAME = jaspProphet
include(InstallModule.pri)

#MODULE_NAME = jaspProcessControl
#include(InstallModule.pri)

MODULE_NAME = jaspCircular
include(InstallModule.pri)

macx:	QMAKE_CLEAN				+=  $$MODULES_RENV_CACHE/* #Dont do this on linux because we are building from source...
win32:	libraryClean.commands	+= rd $$quote($$winPathFix($$MODULES_RENV_CACHE/*)) /S /Q || exit 0;	$$escape_expand(\\n\\t)

#see https://stackoverflow.com/questions/29853832/adding-custom-commands-to-existing-targets-in-qmake
win32 {
	clean.depends          = libraryClean
	QMAKE_EXTRA_TARGETS   += libraryClean clean
}
