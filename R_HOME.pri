LINUX_SPECIAL_CASE = false

_R_HOME = $$(R_HOME)

linux {
	exists(/app/lib/*) {
        _R_HOME = /app/lib64/R
        INCLUDEPATH += /app/lib64/R/library/include
    } else {
		$$LINUX_SPECIAL_CASE {
			_R_HOME = /usr/lib64/R
			INCLUDEPATH += /usr/lib64/R/library/include
		} else {
                        _R_HOME = /usr/lib/
			INCLUDEPATH += /usr/lib/R/library/include
		}
	}

    #QMAKE_CXXFLAGS += -D\'R_HOME=\"$$_R_HOME\"\'
    INCLUDEPATH += /usr/include/R/\
        /usr/share/R/include \
        $$_R_HOME/site-library/Rcpp/include


    R_EXE  = $$_R_HOME/bin/R

    DEFINES += 'R_HOME=\\\"$$_R_HOME\\\"'
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

INCLUDEPATH += \
    $$_R_HOME/library/Rcpp/include \
    $$_R_HOME/include


message(using R_HOME of $$_R_HOME)
