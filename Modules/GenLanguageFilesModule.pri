$$GENERATE_LANGUAGE_FILES {
	
	QM_TRANSLATION_FOLDER	= /inst/qml/translations
	QM_FILE_LOCATION		= $$winPathFix($$PWD/$${MODULE_NAME}$${QM_TRANSLATION_FOLDER})
	MODULE_LOCATION			= $$winPathFix($$PWD/$${MODULE_NAME})
	
	#pretty sure the unix and win32 blocks can be merged into one block with some strategic variable-placements

	win32{ 
		FILE_EXTENSIONS=cpp,qml

		#Create inst/qml/translations folder if not exists
		!exists($${QM_FILE_LOCATION}){
			GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(echo '\"$${QM_FILE_LOCATION}\" does not exits. Creating it.') &&
			GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(md \"$${QM_FILE_LOCATION}\") &&
		}

		#Create po folder if not exists
		!exists($${MODULE_LOCATION}\po){
			GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(echo '\"$${MODULE_LOCATION}\po\" does not exits. Creating it.') &&
			GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(md \"$${MODULE_LOCATION}\po\") &&
		}

		#Update and Cleanup QML-$${MODULE_NAME}.pot file
		GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${WINQTBIN}lupdate.exe\" -locations none -extensions $${FILE_EXTENSIONS} -recursive \"$${MODULE_LOCATION}\" -ts \"$${MODULE_LOCATION}\po\QML-$${MODULE_NAME}.pot\") &&
		GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${GETTEXT_LOCATION}\msgattrib.exe\" --no-obsolete --no-location \"$${MODULE_LOCATION}\po\QML-$${MODULE_NAME}.pot\" -o \"$${MODULE_LOCATION}\po\QML-$${MODULE_NAME}.pot\") &&

		for(LANGUAGE_CODE, SUPPORTED_LANGUAGES) {
			#Busy Message
			GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(echo "Generating language File: $${LANGUAGE_CODE}") &&

			GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${WINQTBIN}lupdate.exe\" -locations none -extensions $${FILE_EXTENSIONS} -recursive \"$${MODULE_LOCATION}\" -ts  \"$${MODULE_LOCATION}\po\QML-$${LANGUAGE_CODE}.po\") &&
			#Cleanup QML-xx.po file
			GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${GETTEXT_LOCATION}\msgattrib.exe\" --no-obsolete --no-location \"$${MODULE_LOCATION}\po\QML-$${LANGUAGE_CODE}.po\" -o \"$${MODULE_LOCATION}\po\QML-$${LANGUAGE_CODE}.po\") &&
			#Create $${MODULE_NAME}-$${LANGUAGE_CODE}.qm
			GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${WINQTBIN}lrelease.exe\" \"$${MODULE_LOCATION}\po\QML-$${LANGUAGE_CODE}.po\" -qm \"$${QM_FILE_LOCATION}\\$${MODULE_NAME}-$${LANGUAGE_CODE}.qm\") &&

		}#End for

		WINPWD=$$winPathFix($$PWD)

		#Create R-$${MODULE_NAME}.mo translation file for all different languages. (Need to add GETTEXT location to PATH environment.)
		GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(\"$${WINPWD}\..\Tools\translate.cmd\" \"$$_R_HOME\bin\" \"$${GETTEXT_LOCATION}\" \"$${WINPWD}\..\Tools\" \"$${MODULE_LOCATION}\" ) &&

		#Ready
		GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(echo 'Ready with language files.')
	}

	unix { 

		#Create inst/qml/translations folder if not exists
		!exists($${QM_FILE_LOCATION}){
			GenerateLanguageFiles$${MODULE_NAME}.commands += echo '\"$${MODULE_NAME}$${QM_TRANSLATION_FOLDER}\" does not exits. Creating it.';
			GenerateLanguageFiles$${MODULE_NAME}.commands += mkdir -p $${QM_FILE_LOCATION} ;
		}

		#Create po folder if not exists
		!exists($MODULE_LOCATION/po){
			GenerateLanguageFiles$${MODULE_NAME}.commands += echo '\"$MODULE_LOCATION/po\" does not exits. Creating it.';
			GenerateLanguageFiles$${MODULE_NAME}.commands += mkdir -p \"$MODULE_LOCATION/po\" ;
		}

		#Update and Cleanup QML-$${MODULE_NAME}.pot file
		GenerateLanguageFiles$${MODULE_NAME}.commands += export PATH=$$EXTENDED_PATH;
		GenerateLanguageFiles$${MODULE_NAME}.commands += lupdate -locations none -extensions cpp,qml -recursive $MODULE_LOCATION -ts $MODULE_LOCATION/po/QML-$${MODULE_NAME}.pot ;
		GenerateLanguageFiles$${MODULE_NAME}.commands += msgattrib --no-obsolete --no-location $MODULE_LOCATION/po/QML-$${MODULE_NAME}.pot -o $MODULE_LOCATION/po/QML-$${MODULE_NAME}.pot ;

		for(LANGUAGE_CODE, SUPPORTED_LANGUAGES) {
			#Busy Message
			GenerateLanguageFiles$${MODULE_NAME}.commands += $$quote(echo "Generating language File: $${LANGUAGE_CODE}");

			#Update and Cleanup QML-xx.po file
			GenerateLanguageFiles$${MODULE_NAME}.commands += lupdate -locations none -extensions cpp,qml -recursive $MODULE_LOCATION -ts $MODULE_LOCATION/po/QML-$${LANGUAGE_CODE}.po ;	
			GenerateLanguageFiles$${MODULE_NAME}.commands += msgattrib --no-obsolete --no-location $MODULE_LOCATION/po/QML-$${LANGUAGE_CODE}.po -o $MODULE_LOCATION/po/QML-$${LANGUAGE_CODE}.po ;

			#Create $${MODULE_NAME}.qm
			GenerateLanguageFiles$${MODULE_NAME}.commands += lrelease $MODULE_LOCATION/po/QML-$${LANGUAGE_CODE}.po -qm $${QM_FILE_LOCATION}/$${MODULE_NAME}-$${LANGUAGE_CODE}.qm ;
			}

		#Create $${MODULE_NAME}.mo translation file. (Need to add GETTEXT location to PATH environment.)
		GenerateLanguageFiles$${MODULE_NAME}.commands +=  Rscript $$PWD/../Tools/translate.R $MODULE_LOCATION ;
	}
}
