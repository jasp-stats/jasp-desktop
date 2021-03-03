QT -= core gui

JASP_BUILDROOT_DIR = $$OUT_PWD/..
SUPPORTED_LANGUAGES = nl de es #Just setting this once is enough...

include(../JASP.pri)
include(../R_HOME.pri)

TEMPLATE = aux
CONFIG -= app_bundle

MODULE_DIR  = $$PWD

################################## Common ##################################
MODULE_NAME = jaspDescriptives
include(InstallModule.pri)

MODULE_NAME = jaspAnova
MODULE_DEPS = jaspDescriptives jaspTTests
include(InstallModule.pri)

MODULE_NAME = jaspFactor
include(InstallModule.pri)

MODULE_NAME = jaspFrequencies
include(InstallModule.pri)

MODULE_NAME = jaspRegression
MODULE_DEPS = jaspDescriptives jaspAnova
include(InstallModule.pri)

MODULE_NAME = jaspTTests
include(InstallModule.pri)

#R_MODULES_INSTALL_DEPENDENCIES = true
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

#R_MODULES_INSTALL_DEPENDENCIES = true
MODULE_NAME = jaspMachineLearning
include(InstallModule.pri)

MODULE_NAME = jaspSummaryStatistics
MODULE_DEPS = jaspFrequencies jaspRegression jaspTTests
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

MODULE_NAME = jaspVisualModeling
include(InstallModule.pri)

MODULE_NAME = jaspLearnBayes
include(InstallModule.pri)

MODULE_NAME = jaspProphet
include(InstallModule.pri)

R_MODULES_INSTALL_DEPENDENCIES = true
MODULE_NAME = jaspProcessControl
MODULE_DEPS = jaspDescriptives
include(InstallModule.pri)

#see https://stackoverflow.com/questions/29853832/adding-custom-commands-to-existing-targets-in-qmake
win32 {
	clean.depends          = libraryClean
	QMAKE_EXTRA_TARGETS   += libraryClean clean
}
