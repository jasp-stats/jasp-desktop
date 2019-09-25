
_R_HOME = $$(R_HOME)

linux {
	exists(/app/lib/*) {
		contains(QMAKE_HOST.arch, x86_64):{
      		_R_HOME = /app/lib64/R
		} else {
			_R_HOME = /app/lib/R
		}
  } else {
    exists(/usr/lib64/R) {
      isEmpty(_R_HOME): _R_HOME = /usr/lib64/R
		} else {
      isEmpty(_R_HOME): _R_HOME = /usr/lib/R
		}
	}

  #QMAKE_CXXFLAGS += -D\'R_HOME=\"$$_R_HOME\"\'
  INCLUDEPATH += $$_R_HOME/library/include  \
      /usr/include/R/                       \
      /usr/share/R/include                  \
      $$_R_HOME/site-library/Rcpp/include

  R_EXE  = $$_R_HOME/bin/R

  DEFINES += 'R_HOME=\\\"$$_R_HOME\\\"'
}

message(using R Frameworks of $$JASP_REQUIRED_FILES/Frameworks)

macx {
		isEmpty(_R_HOME):_R_HOME = $$JASP_REQUIRED_FILES/Frameworks/R.framework/Versions/$$CURRENT_R_VERSION/Resources
        R_EXE  = $$_R_HOME/bin/R
}

windows {
        isEmpty(_R_HOME):_R_HOME = $$OUT_PWD/../R
        R_EXE  = $$_R_HOME/bin/$$ARCH/R
}


INCLUDEPATH += \
    $$_R_HOME/library/Rcpp/include \
    $$_R_HOME/include


message(using R_HOME of $$_R_HOME)
