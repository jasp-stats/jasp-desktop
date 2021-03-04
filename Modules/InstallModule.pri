isEmpty(MODULE_NAME) {
	message(You must specify MODULE_NAME to use InstallModule.pri!)
} else {
	isEmpty(MODULE_DIR) MODULE_DIR=$$PWD
	
	JASP_LIBRARY_DIR    = $${JASP_BUILDROOT_DIR}/Modules/$${MODULE_NAME}
	
	#LOAD_WORKAROUND = true
	include(../R_INSTALL_CMDS.pri)
	#First we remove the installed module to make sure it gets properly update. We leave the library dir to avoid having to install the dependencies all the time.
	#This will just have to get cleaned up by "clean"
	
#	unix:	Install$${MODULE_NAME}.commands        = rm -rf   $$JASP_LIBRARY_DIR/$${MODULE_NAME} && ( [ -d $$JASP_LIBRARY_DIR ] ||  mkdir $$JASP_LIBRARY_DIR ) ;							$$escape_expand(\\n\\t)
#	win32:	Install$${MODULE_NAME}.commands        = IF EXIST $$winPathFix($$JASP_LIBRARY_DIR)\\$${MODULE_NAME}	( rd /s /q $$winPathFix($$JASP_LIBRARY_DIR)\\$${MODULE_NAME} );				$$escape_expand(\\n\\t)
#	win32:  Install$${MODULE_NAME}.commands       += IF NOT EXIST \"$$winPathFix($$JASP_LIBRARY_DIR)\"				( mkdir \"$$winPathFix($$JASP_LIBRARY_DIR)\") ;							$$escape_expand(\\n\\t)
	
	#Install the actual module package
	#Install$${MODULE_NAME}.commands     +=  $${INSTALL_R_PKG_CMD_PREFIX}$${MODULE_DIR}/$${MODULE_NAME}$${INSTALL_R_PKG_CMD_POSTFIX}; $$escape_expand(\\n\\t)
	SETTING_UP_RENV= "Sys.setenv(RENV_PATHS_ROOT=\'$$MODULES_RENV_ROOT\', RENV_PATHS_CACHE=\'$$MODULES_RENV_CACHE\');"
	
	Install$${MODULE_NAME}.commands     +=  $$runRCommandForInstall("$$SETTING_UP_RENV jaspBase::installJaspModule(modulePkg=\'$${MODULE_DIR}/$${MODULE_NAME}\', libPathsToUse=NULL, moduleLibrary=\'$${JASP_LIBRARY_DIR}\', repos=\'https://cloud.r-project.org/\', onlyModPkg=FALSE)" )
	
	#make sure each module is only installed after the previous one, to avoid renv clobbering itself (or just crashing)
	!isEmpty(PREVIOUS_MODULE): Install$${MODULE_NAME}.depends += Install$${PREVIOUS_MODULE}
	
	include(GenLanguageFilesModule.pri)

	QMAKE_EXTRA_TARGETS += Install$${MODULE_NAME}
	POST_TARGETDEPS     += Install$${MODULE_NAME}

	QMAKE_EXTRA_TARGETS += GenerateLanguageFiles$${MODULE_NAME}
	POST_TARGETDEPS     += GenerateLanguageFiles$${MODULE_NAME}


	#See this: https://www.qtcentre.org/threads/9287-How-do-I-get-QMAKE_CLEAN-to-delete-a-directory
	unix:   QMAKE_DEL_FILE = rm -rf
	unix:	QMAKE_CLEAN	  += $$JASP_LIBRARY_DIR/* $$JASP_LIBRARY_DIR/.*

	#we do not use QMAKE_DEL_FILE + QMAKE_CLEAN because otherwise /S and /Q get turned into \\S and \\Q :(
	#see end of Modules.pro for the rest of it
	win32:	libraryClean.commands += rd $$quote($$winPathFix($$JASP_LIBRARY_DIR/*)) /S /Q || exit 0; $$escape_expand(\\n\\t)
	win32:	libraryClean.commands += rd $$quote($$winPathFix($$JASP_LIBRARY_DIR/.*)) /S /Q || exit 0; $$escape_expand(\\n\\t)
	
	#QMAKE_DISTCLEAN	+= $$JASP_LIBRARY_DIR/*/*/* $$JASP_LIBRARY_DIR/*/* $$JASP_LIBRARY_DIR/* $$JASP_LIBRARY_DIR
}

# save current module as previous so we can let the next one depend on this one
PREVIOUS_MODULE = $$MODULE_NAME

#reset the special vars:
MODULE_NAME =
