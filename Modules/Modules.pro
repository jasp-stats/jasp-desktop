QT -= core gui

JASP_BUILDROOT_DIR = $$OUT_PWD/..

include(../JASP.pri)
include(../R_HOME.pri)

TEMPLATE = aux
CONFIG -= app_bundle

MODULE_DIR  = $$PWD

################################## Common ##################################
MODULE_NAME = jaspDescriptives
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

MODULE_NAME = jaspAnova
SUPPORTED_LANGUAGES = nl de es
MODULE_DEPS = jaspDescriptives jaspTTests
include(InstallModule.pri)

MODULE_NAME = jaspFactor
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

MODULE_NAME = jaspFrequencies
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

MODULE_NAME = jaspRegression
SUPPORTED_LANGUAGES = nl de es
MODULE_DEPS = jaspDescriptives jaspAnova
include(InstallModule.pri)

MODULE_NAME = jaspTTests
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

#R_MODULES_INSTALL_DEPENDENCIES = true
MODULE_NAME = jaspMixedModels
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

################################## Extra ##################################
MODULE_NAME = jaspAudit
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

MODULE_NAME = jaspBain
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

MODULE_NAME = jaspNetwork
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

MODULE_NAME = jaspSem
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

#R_MODULES_INSTALL_DEPENDENCIES = true
MODULE_NAME = jaspMachineLearning
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

MODULE_NAME = jaspSummaryStatistics
SUPPORTED_LANGUAGES = nl de es
MODULE_DEPS = jaspFrequencies jaspRegression jaspTTests
include(InstallModule.pri)

MODULE_NAME = jaspMetaAnalysis
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

MODULE_NAME = jaspDistributions
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

MODULE_NAME = jaspEquivalenceTTests
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

MODULE_NAME = jaspJags
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

MODULE_NAME = jaspReliability
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

MODULE_NAME = jaspVisualModeling
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

MODULE_NAME = jaspLearnBayes
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

MODULE_NAME = jaspProphet
SUPPORTED_LANGUAGES = nl de es
include(InstallModule.pri)

#see https://stackoverflow.com/questions/29853832/adding-custom-commands-to-existing-targets-in-qmake
win32 {
	clean.depends          = libraryClean
	QMAKE_EXTRA_TARGETS   += libraryClean clean
}
