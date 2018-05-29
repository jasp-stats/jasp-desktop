LINUX_SPECIAL_CASE = false

_R_HOME = $$(R_HOME)

linux {
    $$LINUX_SPECIAL_CASE {
        _R_HOME = /usr/lib64/R
        INCLUDEPATH += /usr/lib64/R/library/include
    } else {
        _R_HOME = /usr/lib/R
        INCLUDEPATH += /usr/lib/R/library/include
    }

    QMAKE_CXXFLAGS += -D\'R_HOME=\"$$_R_HOME\"\'
    INCLUDEPATH += /usr/include/R/


    R_EXE  = $$_R_HOME/bin/R


    INCLUDEPATH += \
        /usr/share/R/include \
        $$_R_HOME/site-library/Rcpp/include
}

macx {
        isEmpty(_R_HOME):_R_HOME = $$OUT_PWD/../../Frameworks/R.framework/Versions/$$CURRENT_R_VERSION/Resources
        R_EXE  = $$_R_HOME/bin/R
}

windows {
        isEmpty(_R_HOME):_R_HOME = $$OUT_PWD/../R
        R_EXE  = $$_R_HOME/bin/$$ARCH/R
}

$$BUILDING_JASP_ENGINE {
	linux: LIBS += -L$$_R_HOME/lib -lR -lrt # because linux JASP-R-Interface is staticlib
	macx:  LIBS += -L$$_R_HOME/lib -lR
} else {
	
	win32: LIBS += -L$$_R_HOME/bin/$$ARCH -lR
}




message(using R_HOME of $$_R_HOME)
