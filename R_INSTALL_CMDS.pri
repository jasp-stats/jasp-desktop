isEmpty(JASP_BUILDROOT_DIR):  JASP_BUILDROOT_DIR  = $$OUT_PWD/.. #We expect this to be run from at least one folder deep into the buildroot! JASP-Engine/JASP-Desktop/Etc

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
  isEmpty(JASP_LIBRARY_DIR):   JASP_LIBRARY_DIR   = $$_R_HOME/library/
} else {              #by default we should install to the buildroot R library though
  isEmpty(JASP_LIBRARY_DIR):   JASP_LIBRARY_DIR   = $${JASP_BUILDROOT_DIR}/R/library\
}

win32:  INSTALL_R_PKG_CMD_PREFIX  = \"$$R_EXE\" -e \".libPaths(\'$${JASP_BUILDROOT_DIR}/R/library\'); install.packages(\'
unix:   INSTALL_R_PKG_CMD_PREFIX  = \"$$R_EXE\" -e \".libPaths(\'$$_R_HOME/library\'); install.packages(\'

win32:  INSTALL_R_PKG_DEPS_CMD_PREFIX  = \"$$R_EXE\" -e \".libPaths(\'$${JASP_BUILDROOT_DIR}/R/library\'); remotes::install_deps(pkg=\'
unix:   INSTALL_R_PKG_DEPS_CMD_PREFIX  = \"$$R_EXE\" -e \".libPaths(\'$$_R_HOME/library\'); remotes::install_deps(pkg=\'

INSTALL_R_PKG_CMD_POSTFIX      = \', lib=\'$${JASP_LIBRARY_DIR}\', INSTALL_opts=\'--no-multiarch --no-docs --no-test-load\', repos=NULL, type=\'source\')\"
INSTALL_R_PKG_DEPS_CMD_POSTFIX = \', lib=\'$${JASP_LIBRARY_DIR}\', INSTALL_opts=\'--no-multiarch --no-docs --no-test-load\', repos=\'https://cloud.r-project.org/\', upgrade=\'never\', THREADS=1)\"

PKG_LOCK_CMD_PREFIX  = IF exist \"$${JASP_BUILDROOT_DIR}/R/library/
PKG_LOCK_CMD_INFIX   = /\" (rd /s /q \"$${JASP_BUILDROOT_DIR}/R/library/
PKG_LOCK_CMD_POSTFIX = /\" && echo Lock removed!) ELSE (echo No lock found!);
