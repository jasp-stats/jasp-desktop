# This is part of https://github.com/jasp-stats/INTERNAL-jasp/issues/996 and works, but requires me to install V8 because of stupid dependency resolution based on CRAN
# So ive turned it off for now, but if you'd like to use it you can!

UNSET_INSTALL_LATER = false
isEmpty(R_MODULES_INSTALL_DEPENDENCIES) { 
	R_MODULES_INSTALL_DEPENDENCIES = false
	UNSET_INSTALL_LATER=true
}


isEmpty(MODULE_NAME) {
    message(You must specify MODULE_NAME to use InstallModule.pri!)
} else {
    isEmpty(MODULE_DIR) MODULE_DIR=$$PWD

    JASP_LIBRARY_DIR    = $${JASP_BUILDROOT_DIR}/Modules/$${MODULE_NAME}
	JASP_LIBARY_DIR_FIX = $$JASP_LIBRARY_DIR

	win32: JASP_LIBARY_DIR_FIX ~= s,/,\\,g

    LOAD_WORKAROUND = true
	include(../R_INSTALL_CMDS.pri)
	#First we remove the installed module to make sure it gets properly update. We leave the library dir to avoid having to install the dependencies all the time.
	#This will just have to get cleaned up by "clean"

    unix:	Install$${MODULE_NAME}.commands        = rm -rf   $$JASP_LIBRARY_DIR/$${MODULE_NAME} && ( [ -d $$JASP_LIBRARY_DIR ] ||  mkdir $$JASP_LIBRARY_DIR ) ;	$$escape_expand(\\n\\t)
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
	
	############# All Translations related commands ###############
	
	$$GENERATE_LANGUAGE_FILES {
	
	QM_TRANSLATION_FOLDER = /inst/qml/translations
	QM_FILE_LOCATION = $$PWD/$${MODULE_NAME}$${QM_TRANSLATION_FOLDER}
	
	win32{ ################## Windows ##################

		WIN_MODULE_LOCATION = $$PWD/$${MODULE_NAME}
		WIN_MODULE_LOCATION ~= s,/,\\,g
		WIN_QM_FILE_LOCATION = $${QM_FILE_LOCATION}
		WIN_QM_FILE_LOCATION ~= s,/,\\,g
		WINPWD=$$PWD
		WINPWD ~= s,/,\\,g
		FILE_EXTENSIONS=cpp,qml

		#Create inst/qml/translations folder if not exists
		!exists($${WIN_QM_FILE_LOCATION}){
		GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(echo '\"$${WIN_QM_FILE_LOCATION}\" does not exits. Creating it.') &&
		GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(md \"$${WIN_QM_FILE_LOCATION}\") &&
		}

		#Create po folder if not exists
		!exists($${WIN_MODULE_LOCATION}\po){
		GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(echo '\"$${WIN_MODULE_LOCATION}\po\" does not exits. Creating it.') &&
		GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(md \"$${WIN_MODULE_LOCATION}\po\") &&
		}

		#Update and Cleanup QML-$${MODULE_NAME}.pot file
		GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${WINQTBIN}lupdate.exe\" -locations none -extensions $${FILE_EXTENSIONS} -recursive \"$${WIN_MODULE_LOCATION}\" -ts \"$${WIN_MODULE_LOCATION}\po\QML-$${MODULE_NAME}.pot\") &&
		GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${GETTEXT_LOCATION}\msgattrib.exe\" --no-obsolete --no-location \"$${WIN_MODULE_LOCATION}\po\QML-$${MODULE_NAME}.pot\" -o \"$${WIN_MODULE_LOCATION}\po\QML-$${MODULE_NAME}.pot\") &&

		for(LANGUAGE_CODE, SUPPORTED_LANGUAGES) {
			#Busy Message
			GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(echo "Generating language File: $${LANGUAGE_CODE}") &&

			#Initialize R-xx.po from previous language files
			#!exists($${WIN_MODULE_LOCATION}\po\R-$${LANGUAGE_CODE}.po){
			#	GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(echo "INITIALIZE R-TRANSLATION-FILES FOR $${MODULE_NAME} WITH MSGMERGE: $${LANGUAGE_CODE}") &&
			#	GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${WINPWD}\..\Tools\translate.cmd\" \"$$_R_HOME\bin\" \"$${GETTEXT_LOCATION}\" \"$${WINPWD}\..\Tools\" \"$${WIN_MODULE_LOCATION}\" ) &&
			#	GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${GETTEXT_LOCATION}\msgmerge.exe\" \"$${WINPWD}\..\Engine\jaspBase\po\R-$${LANGUAGE_CODE}.po\" \"$${WIN_MODULE_LOCATION}\po\R-$${MODULE_NAME}.pot\" > \"$${WIN_MODULE_LOCATION}\po\R-$${LANGUAGE_CODE}.po\" ) &&
			#	GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${GETTEXT_LOCATION}\msgattrib.exe\" --no-obsolete --no-location \"$${WIN_MODULE_LOCATION}\po\R-$${LANGUAGE_CODE}.po\" -o \"$${WIN_MODULE_LOCATION}\po\R-$${LANGUAGE_CODE}.po\") &&
			#}

			#Initialize QML-xx.po from previous language files
			#!exists($${WIN_MODULE_LOCATION}\po\R-$${LANGUAGE_CODE}.po){
			#	GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(echo "INITIALIZE QML-TRANSLATION-FILES FOR $${MODULE_NAME} WITH MSGMERGE: $${LANGUAGE_CODE}") &&
			#	GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${GETTEXT_LOCATION}\msgmerge.exe\" \"$${WINPWD}\..\Desktop\po\jasp_$${LANGUAGE_CODE}.po\" \"$${WIN_MODULE_LOCATION}\po\QML-$${MODULE_NAME}.pot\" > \"$${WIN_MODULE_LOCATION}\po\QML-$${LANGUAGE_CODE}.po\" ) &&
			#	GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${GETTEXT_LOCATION}\msgattrib.exe\" --no-obsolete --no-location \"$${WIN_MODULE_LOCATION}\po\QML-$${LANGUAGE_CODE}.po\" -o \"$${WIN_MODULE_LOCATION}\po\QML-$${LANGUAGE_CODE}.po\") &&
			#}

			GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${WINQTBIN}lupdate.exe\" -locations none -extensions $${FILE_EXTENSIONS} -recursive \"$${WIN_MODULE_LOCATION}\" -ts  \"$${WIN_MODULE_LOCATION}\po\QML-$${LANGUAGE_CODE}.po\") &&
			#Cleanup QML-xx.po file
			GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${GETTEXT_LOCATION}\msgattrib.exe\" --no-obsolete --no-location \"$${WIN_MODULE_LOCATION}\po\QML-$${LANGUAGE_CODE}.po\" -o \"$${WIN_MODULE_LOCATION}\po\QML-$${LANGUAGE_CODE}.po\") &&
			#Create $${MODULE_NAME}-$${LANGUAGE_CODE}.qm
			GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${WINQTBIN}lrelease.exe\" \"$${WIN_MODULE_LOCATION}\po\QML-$${LANGUAGE_CODE}.po\" -qm \"$${WIN_QM_FILE_LOCATION}\\$${MODULE_NAME}-$${LANGUAGE_CODE}.qm\") &&

		}#End for

		#Create R-$${MODULE_NAME}.mo translation file for all different languages. (Need to add GETTEXT location to PATH environment.)
		GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${WINPWD}\..\Tools\translate.cmd\" \"$$_R_HOME\bin\" \"$${GETTEXT_LOCATION}\" \"$${WINPWD}\..\Tools\" \"$${WIN_MODULE_LOCATION}\" ) &&

		#Ready
		GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(echo 'Ready with language files.')
	}#Win32

	unix{ ################## Unix ##################

		#Create inst/qml/translations folder if not exists
		!exists($${QM_FILE_LOCATION}){
			GenerateLanguageFiles$${MODULE_NAME}.commands += echo '\"$${MODULE_NAME}$${QM_TRANSLATION_FOLDER}\" does not exits. Creating it.';
			GenerateLanguageFiles$${MODULE_NAME}.commands += mkdir -p $${QM_FILE_LOCATION} ;
		}

		#Create po folder if not exists
		!exists($$PWD/$${MODULE_NAME}/po){
			GenerateLanguageFiles$${MODULE_NAME}.commands += echo '\"$$PWD/$${MODULE_NAME}/po\" does not exits. Creating it.';
			GenerateLanguageFiles$${MODULE_NAME}.commands += mkdir -p \"$$PWD/$${MODULE_NAME}/po\" ;
		}

		#Update and Cleanup QML-$${MODULE_NAME}.pot file
		GenerateLanguageFiles$${MODULE_NAME}.commands += export PATH=$$EXTENDED_PATH;
		GenerateLanguageFiles$${MODULE_NAME}.commands += lupdate -locations none -extensions cpp,qml -recursive $$PWD/$${MODULE_NAME} -ts $$PWD/$${MODULE_NAME}/po/QML-$${MODULE_NAME}.pot ;
		GenerateLanguageFiles$${MODULE_NAME}.commands += msgattrib --no-obsolete --no-location $$PWD/$${MODULE_NAME}/po/QML-$${MODULE_NAME}.pot -o $$PWD/$${MODULE_NAME}/po/QML-$${MODULE_NAME}.pot ;

		for(LANGUAGE_CODE, SUPPORTED_LANGUAGES) {
			#Busy Message
			GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(echo "Generating language File: $${LANGUAGE_CODE}");

			#Initialize R-xx.po from previous language files
			#!exists($$PWD/$${MODULE_NAME}/po/R-$${LANGUAGE_CODE}.po){
			#	GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(echo "INITIALIZE R-TRANSLATION-FILES FOR $${MODULE_NAME} WITH MSGMERGE: $${LANGUAGE_CODE}");
			#	GenerateLanguageFiles$${MODULE_NAME}.commands +=  Rscript $$PWD/../Tools/translate.R $$PWD/$${MODULE_NAME} ;
			#	GenerateLanguageFiles$${MODULE_NAME}.commands +=  msgmerge $$PWD/../Engine/jaspBase/po/R-$${LANGUAGE_CODE}.po.0.14.1 $$PWD/$${MODULE_NAME}/po/R-$${MODULE_NAME}.pot > $$PWD/$${MODULE_NAME}/po/R-$${LANGUAGE_CODE}.po ;
			#	GenerateLanguageFiles$${MODULE_NAME}.commands += msgattrib --no-obsolete --no-location $$PWD/$${MODULE_NAME}/po/R-$${LANGUAGE_CODE}.po -o $$PWD/$${MODULE_NAME}/po/R-$${LANGUAGE_CODE}.po ;
			#}

			#Initialize QML-xx.po from previous language files
			#!exists($$PWD/$${MODULE_NAME}/po/QML-$${LANGUAGE_CODE}.po){
			#	GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(echo "INITIALIZE QML-TRANSLATION-FILES FOR $${MODULE_NAME} WITH MSGMERGE: $${LANGUAGE_CODE}");
			#	GenerateLanguageFiles$${MODULE_NAME}.commands +=  msgmerge $$PWD/../Desktop/po/jasp_$${LANGUAGE_CODE}.po $$PWD/$${MODULE_NAME}/po/QML-$${MODULE_NAME}.pot > $$PWD/$${MODULE_NAME}/po/QML-$${LANGUAGE_CODE}.po ;
			#	GenerateLanguageFiles$${MODULE_NAME}.commands += msgattrib --no-obsolete --no-location $$PWD/$${MODULE_NAME}/po/QML-$${LANGUAGE_CODE}.po -o $$PWD/$${MODULE_NAME}/po/QML-$${LANGUAGE_CODE}.po ;
			#}

			#Update and Cleanup QML-xx.po file
			GenerateLanguageFiles$${MODULE_NAME}.commands += lupdate -locations none -extensions cpp,qml -recursive $$PWD/$${MODULE_NAME} -ts $$PWD/$${MODULE_NAME}/po/QML-$${LANGUAGE_CODE}.po ;	
			GenerateLanguageFiles$${MODULE_NAME}.commands += msgattrib --no-obsolete --no-location $$PWD/$${MODULE_NAME}/po/QML-$${LANGUAGE_CODE}.po -o $$PWD/$${MODULE_NAME}/po/QML-$${LANGUAGE_CODE}.po ;

			#Create $${MODULE_NAME}.qm
			GenerateLanguageFiles$${MODULE_NAME}.commands += lrelease $$PWD/$${MODULE_NAME}/po/QML-$${LANGUAGE_CODE}.po -qm $${QM_FILE_LOCATION}/$${MODULE_NAME}-$${LANGUAGE_CODE}.qm ;
			}

		#Create $${MODULE_NAME}.mo translation file. (Need to add GETTEXT location to PATH environment.)
		GenerateLanguageFiles$${MODULE_NAME}.commands +=  Rscript $$PWD/../Tools/translate.R $$PWD/$${MODULE_NAME} ;
	}#Unix
	}#########################################################

		QMAKE_EXTRA_TARGETS += Install$${MODULE_NAME}
		POST_TARGETDEPS     += Install$${MODULE_NAME}

		QMAKE_EXTRA_TARGETS += PostInstallFix$${MODULE_NAME}
		POST_TARGETDEPS     += PostInstallFix$${MODULE_NAME}

		QMAKE_EXTRA_TARGETS += GenerateLanguageFiles$${MODULE_NAME}
		POST_TARGETDEPS     += GenerateLanguageFiles$${MODULE_NAME}


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

$$UNSET_INSTALL_LATER: R_MODULES_INSTALL_DEPENDENCIES =
