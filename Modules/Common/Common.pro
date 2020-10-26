QT -= core gui

JASP_BUILDROOT_DIR = $$OUT_PWD/../..

include(../../JASP.pri)
include(../../R_HOME.pri)

TEMPLATE = aux
CONFIG -= app_bundle

MODULE_DIR  = $$PWD

MODULE_NAME = jaspDescriptives
include(../InstallModule.pri)

MODULE_NAME = jaspAnova
MODULE_DEPS = jaspDescriptives jaspTTests
include(../InstallModule.pri)

MODULE_NAME = jaspFactor
include(../InstallModule.pri)

MODULE_NAME = jaspFrequencies
include(../InstallModule.pri)

MODULE_NAME = jaspRegression
MODULE_DEPS = jaspDescriptives jaspAnova
include(../InstallModule.pri)

MODULE_NAME = jaspTTests
include(../InstallModule.pri)

MODULE_NAME = jaspMixedModels
include(../InstallModule.pri)

