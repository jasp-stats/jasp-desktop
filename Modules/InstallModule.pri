# This is part of https://github.com/jasp-stats/INTERNAL-jasp/issues/996 and works, but requires me to install V8 because of stupid dependency resolution based on CRAN
# So ive turned it off for now, but if you'd like to use it you can!
isEmpty(R_MODULES_INSTALL_DEPENDENCIES) { 
	R_MODULES_INSTALL_DEPENDENCIES = false
}


isEmpty(MODULE_NAME) {
    message(You must specify MODULE_NAME to use InstallModule.pri!)
} else {
    isEmpty(MODULE_DIR) MODULE_DIR=$$PWD

    JASP_LIBRARY_DIR    = $${JASP_BUILDROOT_DIR}/Modules/$${MODULE_NAME}
	JASP_LIBARY_DIR_FIX = $$JASP_LIBRARY_DIR

	win32: JASP_LIBARY_DIR_FIX ~= s,/,\\,g

	include(../R_INSTALL_CMDS.pri)
	#First we remove the installed module to make sure it gets properly update. We leave the library dir to avoid having to install the dependencies all the time.
	#This will just have to get cleaned up by "clean"

	unix:	Install$${MODULE_NAME}.commands        =  rm -rf   $$JASP_LIBRARY_DIR/$${MODULE_NAME} && ( [ -d $$JASP_LIBRARY_DIR ] ||  mkdir $$JASP_LIBRARY_DIR ) ;	$$escape_expand(\\n\\t)
	win32:	Install$${MODULE_NAME}.commands        = IF EXIST $$JASP_LIBARY_DIR_FIX\\$${MODULE_NAME}	( rd /s /q $$JASP_LIBARY_DIR_FIX\\$${MODULE_NAME} );		$$escape_expand(\\n\\t)
	win32:  Install$${MODULE_NAME}.commands       += IF NOT EXIST \"$$JASP_LIBARY_DIR_FIX\"				( mkdir \"$$JASP_LIBARY_DIR_FIX\") ;						$$escape_expand(\\n\\t)

    #Then, if we so desire, we install all dependencies (that are missing anyhow)
	$$R_MODULES_INSTALL_DEPENDENCIES:		Install$${MODULE_NAME}.commands		+= $${INSTALL_R_PKG_DEPS_CMD_PREFIX}$${MODULE_DIR}/$${MODULE_NAME}$${INSTALL_R_PKG_DEPS_CMD_POSTFIX} $$escape_expand(\\n\\t) 
	
    #Install the actual module package
	Install$${MODULE_NAME}.commands     +=  $${INSTALL_R_PKG_CMD_PREFIX}$${MODULE_DIR}/$${MODULE_NAME}$${INSTALL_R_PKG_CMD_POSTFIX}; $$escape_expand(\\n\\t)

    #And lastly we do some postprocessing (on mac this includes fixing any and all necessary paths in dylib's and so's)
	win32 {	
		JASP_BUILDROOT_DIR_FIX				  = $$JASP_BUILDROOT_DIR
		JASP_BUILDROOT_DIR_FIX				 ~= s,/,\\,g
		#PostInstallFix$${MODULE_NAME}.commands		 +=  PUSHD \"$${R_BIN}\" ; $$escape_expand(\\n)
		PostInstallFix$${MODULE_NAME}.commands		 +=  PATH=\"$${R_BIN};%PATH%\" 
		PostInstallFix$${MODULE_NAME}.commands      +=  $${JASP_BUILDROOT_DIR_FIX}\\JASPEngine.exe \"$$JASP_LIBRARY_DIR\" ;	$$escape_expand(\\n\\t)
		#PostInstallFix$${MODULE_NAME}.commands      +=   && echo \"I managed to actually run jaspengine probably?\";			$$escape_expand(\\n\\t)
		#PostInstallFix$${MODULE_NAME}.commands		 +=  POPD ; $$escape_expand(\\n)
	}
	unix:  PostInstallFix$${MODULE_NAME}.commands       +=  LD_LIBRARY_PATH=$${_R_HOME}/lib $${JASP_BUILDROOT_DIR}/JASPEngine $$JASP_LIBRARY_DIR ;				$$escape_expand(\\n\\t)

	PostInstallFix$${MODULE_NAME}.depends = Install$${MODULE_NAME}

    QMAKE_EXTRA_TARGETS += Install$${MODULE_NAME}
	POST_TARGETDEPS     += Install$${MODULE_NAME}

    QMAKE_EXTRA_TARGETS += PostInstallFix$${MODULE_NAME}
	POST_TARGETDEPS     += PostInstallFix$${MODULE_NAME}
    


    #See this: https://www.qtcentre.org/threads/9287-How-do-I-get-QMAKE_CLEAN-to-delete-a-directory
	unix:  QMAKE_DEL_FILE = rm -rf
    unix:	QMAKE_CLEAN			  += $$JASP_LIBRARY_DIR/$${MODULE_NAME}/* $$JASP_LIBRARY_DIR/*
	
	#we do not use QMAKE_DEL_FILE + QMAKE_CLEAN because otherwise /S and /Q get turned into \\S and \\Q :(
	#see end of Modules.pro for the rest of it
	win32:	libraryClean.commands += rd $$quote($$JASP_LIBARY_DIR_FIX) /S /Q || exit 0; $$escape_expand(\\n\\t)
	
	#QMAKE_DISTCLEAN	+= $$JASP_LIBRARY_DIR/*/*/* $$JASP_LIBRARY_DIR/*/* $$JASP_LIBRARY_DIR/* $$JASP_LIBRARY_DIR

	#Make sure we install the r pkgs in the right order.
	for(DEP, MODULE_DEPS) {
		Install$${MODULE_NAME}.depends	+= PostInstallFix$${DEP}
	}
}

#R_MODULES_INSTALL_DEPENDENCIES = false

#reset the special vars:
MODULE_NAME =
MODULE_DEPS = 
R_MODULES_INSTALL_DEPENDENCIES = false
