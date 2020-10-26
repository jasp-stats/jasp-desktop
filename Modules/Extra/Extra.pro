QT -= core gui

JASP_BUILDROOT_DIR = $$OUT_PWD/../..

include(../../JASP.pri)
include(../../R_HOME.pri)

TEMPLATE = aux
CONFIG -= app_bundle

MODULE_DIR  = $$PWD
MODULE_NAME = jaspAudit
include(../InstallModule.pri)

MODULE_NAME = jaspBain
include(../InstallModule.pri)

MODULE_NAME = jaspNetwork
include(../InstallModule.pri)

MODULE_NAME = jaspSem
include(../InstallModule.pri)

R_MODULES_INSTALL_DEPENDENCIES = true
MODULE_NAME = jaspMachineLearning
include(../InstallModule.pri)
R_MODULES_INSTALL_DEPENDENCIES = false

MODULE_NAME = jaspSummaryStatistics
MODULE_DEPS = jaspFrequencies jaspRegression jaspTTests
include(../InstallModule.pri)

MODULE_NAME = jaspMetaAnalysis
include(../InstallModule.pri)

MODULE_NAME = jaspDistributions
include(../InstallModule.pri)

MODULE_NAME = jaspEquivalenceTTests
include(../InstallModule.pri)

MODULE_NAME = jaspJags
include(../InstallModule.pri)

MODULE_NAME = jaspReliability
include(../InstallModule.pri)

MODULE_NAME = jaspVisualModeling
include(../InstallModule.pri)

MODULE_NAME = jaspLearnBayes
include(../InstallModule.pri)
