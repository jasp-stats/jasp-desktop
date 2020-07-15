QT -= core gui

JASP_BUILDROOT_DIR = $$OUT_PWD/../..
include(../../JASP.pri)
include(../../R_HOME.pri)

TEMPLATE = aux
CONFIG -= app_bundle

MODULE_DIR  = $$PWD
MODULE_NAME = Audit
include(../InstallModule.pri)

MODULE_NAME = BAIN
include(../InstallModule.pri)

MODULE_NAME = Network
include(../InstallModule.pri)

MODULE_NAME = SEM
include(../InstallModule.pri)

MODULE_NAME = MachineLearning
include(../InstallModule.pri)

MODULE_NAME = SummaryStatistics
include(../InstallModule.pri)

MODULE_NAME = MetaAnalysis
include(../InstallModule.pri)

MODULE_NAME = DiscoverDistributions
include(../InstallModule.pri)

MODULE_NAME = EquivalenceTTests
include(../InstallModule.pri)

MODULE_NAME = JAGS
include(../InstallModule.pri)

MODULE_NAME = Reliability
include(../InstallModule.pri)

MODULE_NAME = VisualModeling
include(../InstallModule.pri)

