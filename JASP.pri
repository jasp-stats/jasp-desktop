#Used for common JASP qmake build settings

#Jasp-R-Interface
JASP_R_INTERFACE_TARGET = JASP-R-Interface
JASP_R_INTERFACE_MAJOR_VERSION = 1 # Interface changes
JASP_R_INTERFACE_MINOR_VERSION = 0 # Code changes
JASP_R_INTERFACE_NAME = $$JASP_R_INTERFACE_TARGET$$JASP_R_INTERFACE_MAJOR_VERSION'.'$$JASP_R_INTERFACE_MINOR_VERSION

#R settings
CURRENT_R_VERSION = 3.4
DEFINES += "CURRENT_R_VERSION=\"$$CURRENT_R_VERSION\""

LINUX_SPECIAL_CASE = false

linux {
    _R_HOME = $$(R_HOME)

    $$LINUX_SPECIAL_CASE {
        _R_HOME = /usr/lib64/R
        INCLUDEPATH += /usr/lib64/R/library/include
        DEFINES += LIBJSON_DIR_UP
    } else {
        _R_HOME = /usr/lib/R
        INCLUDEPATH += /usr/lib/R/library/include
    }

    QMAKE_CXXFLAGS += -D\'R_HOME=\"$$_R_HOME\"\'
    INCLUDEPATH += /usr/include/R/

    LIBS += -L$$_R_HOME/lib -lR -lrt
    R_EXE  = $$_R_HOME/bin/R


    INCLUDEPATH += \
        /usr/share/R/include \
        $$_R_HOME/site-library/Rcpp/include
} else {
    _R_HOME = $$(R_HOME)
}

message(using R_HOME of $$_R_HOME)
