isEmpty(JASP_BUILDROOT_DIR):  JASP_BUILDROOT_DIR  = $$OUT_PWD/.. #We expect this to be run from at least one folder deep into the buildroot! Engine/Desktop/Etc

macx {
        isEmpty(_R_HOME):_R_HOME = $${JASP_BUILDROOT_DIR}/../Frameworks/R.framework/Versions/$$CURRENT_R_VERSION/Resources
        R_EXE  = $$_R_HOME/bin/R
}

windows {
        isEmpty(_R_HOME):_R_HOME = $${JASP_BUILDROOT_DIR}/R
        R_BIN  = $$winPathFix($$_R_HOME/bin/$$ARCH)
		R_EXE  = $$R_BIN\\R
}

exists(/app/lib/*) {  #for flatpak we can should use R's own library as it is contained anyway
  ROOT_LIBRARY_DIR   = $$_R_HOME/library/
} else {              #by default we should install to the buildroot R library though
  ROOT_LIBRARY_DIR   = $${JASP_BUILDROOT_DIR}/R/library\
}

isEmpty(JASP_LIBRARY_DIR):   JASP_LIBRARY_DIR   = $$ROOT_LIBRARY_DIR

win32:  LIBPATHS = ".libPaths(c(\'$$ROOT_LIBRARY_DIR\', \'$${JASP_BUILDROOT_DIR}/R/library\'))"
unix:	LIBPATHS = ".libPaths(c(\'$$ROOT_LIBRARY_DIR\', \'$$_R_HOME/library\'))"

isEmpty(LOAD_WORKAROUND): LOAD_WORKAROUND = false
WORKAROUND_LOADER =
$$LOAD_WORKAROUND: WORKAROUND_LOADER = source(\'../workarounds.R\');

INSTALL_R_PKG_PREFIX_PREFIX		= \"$$R_EXE\" -e \"$$LIBPATHS; $$WORKAROUND_LOADER Sys.setenv(\'R_REMOTES_NO_ERRORS_FROM_WARNINGS\'=TRUE); pkgbuild::with_build_tools( \{ 

mac {
	INSTALL_R_PKG_PREFIX_PREFIX		= JASP_R_HOME=\"$$_R_HOME\" $$INSTALL_R_PKG_PREFIX_PREFIX
}

#everything below here probably wont be necessary, as in fact the whole extraLibPaths thing at the top...

INSTALL_R_PKG_CMD_PREFIX		= $$INSTALL_R_PKG_PREFIX_PREFIX install.packages(\'
INSTALL_R_PKG_DEPS_CMD_PREFIX	= $$INSTALL_R_PKG_PREFIX_PREFIX remotes::install_deps(pkg=\'

isEmpty(DEPS_LIBRARY_DIR)
{
    isEmpty(INSTALL_DEPS_TO_ROOT_LIBRARY) { DEPS_LIBRARY_DIR=$${JASP_LIBRARY_DIR}
	} else {								DEPS_LIBRARY_DIR=$${ROOT_LIBRARY_DIR} }
}

INSTALL_R_PKG_POSTFIX_POSTFIX  =  \}, required=FALSE )\"

INSTALL_R_PKG_CMD_POSTFIX      = \', lib=\'$${JASP_LIBRARY_DIR}\', INSTALL_opts=\'--no-multiarch --no-docs --no-test-load\', repos=NULL, type=\'source\') $$INSTALL_R_PKG_POSTFIX_POSTFIX
INSTALL_R_PKG_DEPS_CMD_POSTFIX = \', lib=\'$${DEPS_LIBRARY_DIR}\', INSTALL_opts=\'--no-multiarch --no-docs --no-test-load\', repos=\'https://cloud.r-project.org/\', upgrade=\'never\', THREADS=1) $$INSTALL_R_PKG_POSTFIX_POSTFIX

PKG_LOCK_CMD_PREFIX  = IF exist \"$${JASP_BUILDROOT_DIR}/R/library/
PKG_LOCK_CMD_INFIX   = /\" (rd /s /q \"$${JASP_BUILDROOT_DIR}/R/library/
PKG_LOCK_CMD_POSTFIX = /\" && echo Lock removed!) ELSE (echo No lock found!);

defineReplace(runRCommandForInstall) {
	CMD = $$1
	return($${INSTALL_R_PKG_PREFIX_PREFIX} $$CMD $${INSTALL_R_PKG_POSTFIX_POSTFIX})
}
