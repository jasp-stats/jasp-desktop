QT -= core gui

JASP_BUILDROOT_DIR = $$OUT_PWD/../..
include(../../JASP.pri)
include(../../R_HOME.pri)

TEMPLATE = aux
CONFIG -= app_bundle

MODULE_DIR  = $$PWD

MODULE_NAME = Descriptives
include(../InstallModule.pri)

MODULE_NAME = ANOVA
include(../InstallModule.pri)

MODULE_NAME = Factor
include(../InstallModule.pri)

MODULE_NAME = Frequencies
include(../InstallModule.pri)

MODULE_NAME = Regression
include(../InstallModule.pri)

MODULE_NAME = TTests
include(../InstallModule.pri)

MODULE_NAME = MixedModels
include(../InstallModule.pri)

