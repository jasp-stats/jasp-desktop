isEmpty(JASP_BUILDROOT_DIR):  JASP_BUILDROOT_DIR  = $$OUT_PWD/.. #We expect this to be run from at least one folder deep into the buildroot! Engine/Desktop/Etc

macx {
        isEmpty(_R_HOME):_R_HOME = $${JASP_BUILDROOT_DIR}/../Frameworks/R.framework/Versions/$$CURRENT_R_VERSION/Resources
        R_EXE  = $$_R_HOME/bin/R
}

windows {
        isEmpty(_R_HOME):_R_HOME = $${JASP_BUILDROOT_DIR}/R
        R_BIN  = $$_R_HOME/bin/$$ARCH
		R_BIN ~= s,/,\\,g        
		R_EXE  = $$R_BIN\\R
}

exists(/app/lib/*) {  #for flatpak we can should use R's own library as it is contained anyway
  ROOT_LIBRARY_DIR   = $$_R_HOME/library/
} else {              #by default we should install to the buildroot R library though
  ROOT_LIBRARY_DIR   = $${JASP_BUILDROOT_DIR}/R/library\
}

isEmpty(JASP_LIBRARY_DIR):   JASP_LIBRARY_DIR   = $$ROOT_LIBRARY_DIR


defineReplace(generateExtraLibPaths) {
	DEPS_VAR	= $$1
	DEPS		= $$eval($$DEPS_VAR)
	EXTRA_LIBS	= 

	#The comma even when EXTRA_LIBS is empty is intentional, to make sure it fits right into LIBPATHS if there is at least 1 dep
	for(DEP, DEPS) {
		EXTRA_LIBS = $${EXTRA_LIBS}, \'$${JASP_BUILDROOT_DIR}/Modules/$${DEP}\'
	}

	return($$EXTRA_LIBS)
}

#MODULE_DEPS can be used to define which other modules this one is dependent on.

win32:  LIBPATHS = ".libPaths(c(\'$$ROOT_LIBRARY_DIR\', \'$${JASP_BUILDROOT_DIR}/R/library\'$$generateExtraLibPaths(MODULE_DEPS)))"
unix:	LIBPATHS = ".libPaths(c(\'$$ROOT_LIBRARY_DIR\', \'$$_R_HOME/library\'$$generateExtraLibPaths(MODULE_DEPS)))"

INSTALL_R_PKG_CMD_PREFIX		= \"$$R_EXE\" -e \"$$LIBPATHS; install.packages(\'
INSTALL_R_PKG_DEPS_CMD_PREFIX	= \"$$R_EXE\" -e \"$$LIBPATHS; remotes::install_deps(pkg=\'

mac {
	INSTALL_R_PKG_CMD_PREFIX		= JASP_R_HOME=\"$$_R_HOME\" $$INSTALL_R_PKG_CMD_PREFIX
	INSTALL_R_PKG_DEPS_CMD_PREFIX	= JASP_R_HOME=\"$$_R_HOME\" $$INSTALL_R_PKG_DEPS_CMD_PREFIX
}

INSTALL_R_PKG_CMD_POSTFIX      = \', lib=\'$${JASP_LIBRARY_DIR}\', INSTALL_opts=\'--no-multiarch --no-docs --no-test-load\', repos=NULL, type=\'source\')\"
INSTALL_R_PKG_DEPS_CMD_POSTFIX = \', lib=\'$${JASP_LIBRARY_DIR}\', INSTALL_opts=\'--no-multiarch --no-docs --no-test-load\', repos=\'https://cloud.r-project.org/\', upgrade=\'never\', THREADS=1)\"

PKG_LOCK_CMD_PREFIX  = IF exist \"$${JASP_BUILDROOT_DIR}/R/library/
PKG_LOCK_CMD_INFIX   = /\" (rd /s /q \"$${JASP_BUILDROOT_DIR}/R/library/
PKG_LOCK_CMD_POSTFIX = /\" && echo Lock removed!) ELSE (echo No lock found!);
