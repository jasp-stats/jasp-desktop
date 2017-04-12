############################################################################################
#      NSIS Installation Script created by Damian Dropmann             
############################################################################################

!include "nsProcess.nsh"
!include "x64.nsh"

!define MAIN_VERSION "0.8.1"
!define PATCH_NUM "1"
!define VERSION "${MAIN_VERSION}.${PATCH_NUM}"
!define BETA ""
!define APP_NAME "JASP"
!define INSTALLER_NAME "${APP_NAME}-${VERSION}-Setup.exe"
!define CONTENTS_DIR "C:\Jasp\Install"
!define APP_DISPLAY_NAME "${APP_NAME} ${VERSION}"
!define COMP_NAME "The ${APP_NAME} Statistics Project"
!define WEB_SITE "https://jasp-stats.org"
!define COPYRIGHT "${APP_NAME} © 2017"
!define DESCRIPTION "${APP_NAME} - A Fresh Way to Do Statistics"
!define LICENSE_TXT "${CONTENTS_DIR}\AGPL.txt"
!define MAIN_APP_EXE "${APP_NAME}.exe"
!define MAIN_APP_ICO "${APP_NAME}.ico"
!define INSTALL_TYPE "SetShellVarContext all"
!define REG_ROOT "HKLM"
!define VERSION_TRACKING "SOFTWARE\${APP_NAME}\Versions"
!define REG_APP_PATH "Software\Microsoft\Windows\CurrentVersion\App Paths\${MAIN_APP_EXE}"
!define UNINSTALL_BASEPATH "Software\Microsoft\Windows\CurrentVersion\Uninstall"
!define UNINSTALL_PATH "${UNINSTALL_BASEPATH}\${APP_DISPLAY_NAME}"
!define FILE_EXTENSION ".jasp"
!define OPEN_CMD_PATH "${APP_NAME}\Shell\open\command"
!define ICON_REG_PATH "${APP_NAME}\DefaultIcon"


######################################################################

VIProductVersion  "${VERSION}"
VIAddVersionKey "ProductName"  "${APP_DISPLAY_NAME}"
VIAddVersionKey "CompanyName"  "${COMP_NAME}"
VIAddVersionKey "LegalCopyright"  "${COPYRIGHT}"
VIAddVersionKey "FileDescription"  "${DESCRIPTION}"
VIAddVersionKey "FileVersion"  "${VERSION}"

######################################################################

SetCompressor ZLIB
Name "${APP_DISPLAY_NAME}"
Caption "${APP_DISPLAY_NAME}"
OutFile "${INSTALLER_NAME}"
BrandingText "${APP_DISPLAY_NAME}"
XPStyle on
ShowInstDetails show
;InstallDirRegKey "${REG_ROOT}" "${REG_APP_PATH}" ""
InstallDir "$PROGRAMFILES64\${APP_DISPLAY_NAME}"

######################################################################

!include "MUI.nsh"

!define MUI_ABORTWARNING
!define MUI_UNABORTWARNING

!insertmacro MUI_PAGE_WELCOME

!ifdef LICENSE_TXT
!insertmacro MUI_PAGE_LICENSE "${LICENSE_TXT}"
!endif

!ifdef REG_START_MENU
!define MUI_STARTMENUPAGE_NODISABLE
!define MUI_STARTMENUPAGE_DEFAULTFOLDER "${APP_DISPLAY_NAME}"
!define MUI_STARTMENUPAGE_REGISTRY_ROOT "${REG_ROOT}"
!define MUI_STARTMENUPAGE_REGISTRY_KEY "${UNINSTALL_PATH}"
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "${REG_START_MENU}"
!insertmacro MUI_PAGE_STARTMENU Application $SM_Folder
!endif

!insertmacro MUI_PAGE_INSTFILES

!define MUI_FINISHPAGE_RUN "$INSTDIR\${MAIN_APP_EXE}"
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_CONFIRM

!insertmacro MUI_UNPAGE_INSTFILES

!insertmacro MUI_UNPAGE_FINISH

!insertmacro MUI_LANGUAGE "English"

!system 'installer-maker.exe /S /D=${CONTENTS_DIR}'

######################################################################


Function .onInit
	UserInfo::GetAccountType
	pop $0
	${If} $0 != "admin" ;Require admin rights on NT4+
		MessageBox MB_ICONSTOP "Administrator rights required!"
		SetErrorLevel 740 ;ERROR_ELEVATION_REQUIRED
		Quit
	${EndIf}

	${If} ${RunningX64}
		StrCpy $InstDir "$PROGRAMFILES64\${APP_DISPLAY_NAME}"
	${else}
		StrCpy $InstDir "$PROGRAMFILES\${APP_DISPLAY_NAME}"
	${EndIf}

	Var /GLOBAL lastVersion
	Var /GLOBAL versionCount
	Var /GLOBAL versionInstallDir
	
 	StrCpy $versionCount 0
	loop:
		EnumRegKey $1 ${REG_ROOT} "${VERSION_TRACKING}" $versionCount
		StrCmp $1 "" break1
		StrCmp $1 "${APP_DISPLAY_NAME}" alreadyInstalled
		StrCpy $lastVersion $1
		IntOp $versionCount $versionCount + 1
		Goto loop
	break1:
 
	ReadRegStr $versionInstallDir ${REG_ROOT} "${VERSION_TRACKING}\$lastVersion" ""
 
	StrCmp "$versionCount" 1 ask done
 
 alreadyInstalled:
	MessageBox MB_OK|MB_ICONEXCLAMATION "${APP_DISPLAY_NAME} is already installed."
	Abort
 
 ask:
	ReadRegStr $R0 ${REG_ROOT} "${UNINSTALL_BASEPATH}\$lastVersion" "UninstallString"
	StrCmp "$R0" "" done
	ReadRegStr $R1 ${REG_ROOT} "${UNINSTALL_BASEPATH}\$lastVersion" "DisplayName"
 
	MessageBox MB_YESNOCANCEL|MB_ICONEXCLAMATION "Another version of this product ($R1) is already installed. Would you like to remove it before continuing with installation of ${APP_DISPLAY_NAME}? $\n$\nClick `Yes` to uninstall previous version.$\nClick `No` to continue with installation.$\nClick `Cancel` to abort." IDYES uninst IDNO done
	Abort
 
uninst:
	ClearErrors
	
	ExecWait '$R0 _?=$versionInstallDir'
	;ExecWait '$R0 _?=$INSTDIR' ;Do not copy the uninstaller to a temp file
 
	IfErrors no_remove_uninstaller
		Delete $R0
		RmDir $versionInstallDir
		;You can either use Delete /REBOOTOK in the uninstaller or add some code
		;here to remove the uninstaller. Use a registry key to check
		;whether the user has chosen to uninstall. If you are using an uninstaller
		;components page, make sure all sections are uninstalled.
no_remove_uninstaller:
 
done:
 
FunctionEnd

Function un.onInit
	${If} ${RunningX64}
		StrCpy $InstDir "$PROGRAMFILES64\${APP_DISPLAY_NAME}"
	${else}
		StrCpy $InstDir "$PROGRAMFILES\${APP_DISPLAY_NAME}"
	${EndIf}
	
	${nsProcess::FindProcess} "${MAIN_APP_EXE}" $R0
	StrCmp $R0 0 0 notRunning
	MessageBox MB_OK|MB_ICONEXCLAMATION "Cannot continue while ${APP_NAME} is running. Please close all ${APP_NAME} windows and try again." IDOK
		Abort
		
notRunning:

FunctionEnd

Section -MainProgram
${INSTALL_TYPE}
SetOverwrite ifnewer

${If} ${RunningX64}
	!include includeFiles64.nsi
${else}
	!include includeFiles32.nsi
${EndIf}

SectionEnd

######################################################################

Section -Icons_Reg
SetOutPath "$INSTDIR"
WriteUninstaller "$INSTDIR\uninstall.exe"

CreateShortCut "$SMPROGRAMS\${APP_DISPLAY_NAME}.lnk" "$INSTDIR\${MAIN_APP_EXE}"

WriteRegStr ${REG_ROOT} "${REG_APP_PATH}" "" "$INSTDIR\${MAIN_APP_EXE}"
WriteRegStr ${REG_ROOT} "${UNINSTALL_PATH}"  "DisplayName" "${APP_DISPLAY_NAME}"
WriteRegStr ${REG_ROOT} "${UNINSTALL_PATH}"  "UninstallString" "$INSTDIR\uninstall.exe"
WriteRegStr ${REG_ROOT} "${UNINSTALL_PATH}"  "DisplayIcon" "$INSTDIR\${MAIN_APP_EXE}"
WriteRegStr ${REG_ROOT} "${UNINSTALL_PATH}"  "DisplayVersion" "${VERSION}"
WriteRegStr ${REG_ROOT} "${UNINSTALL_PATH}"  "Publisher" "${COMP_NAME}"

!ifdef WEB_SITE
WriteRegStr ${REG_ROOT} "${UNINSTALL_PATH}"  "URLInfoAbout" "${WEB_SITE}"
!endif

WriteRegStr ${REG_ROOT} "${VERSION_TRACKING}\${APP_DISPLAY_NAME}" "" "$INSTDIR"

WriteRegStr HKCR "${FILE_EXTENSION}" "" "${APP_NAME}"
WriteRegStr HKCR "${OPEN_CMD_PATH}" "" '$INSTDIR\${MAIN_APP_EXE} "%1"'
WriteRegStr HKCR "${ICON_REG_PATH}" "" "$INSTDIR\${MAIN_APP_ICO}"

SectionEnd

######################################################################

Section Uninstall
${INSTALL_TYPE}

${If} ${RunningX64}
	!include deleteFiles64.nsi
${else}
	!include deleteFiles32.nsi
${EndIf}
 
Delete "$INSTDIR\uninstall.exe"
RmDir "$INSTDIR"

Delete "$SMPROGRAMS\${APP_DISPLAY_NAME}.lnk"

DeleteRegKey ${REG_ROOT} "${REG_APP_PATH}"
DeleteRegKey ${REG_ROOT} "${UNINSTALL_PATH}"

DeleteRegKey HKCR "${OPEN_CMD_PATH}"
DeleteRegKey HKCR "${ICON_REG_PATH}"
DeleteRegKey HKCR "${FILE_EXTENSION}"

DeleteRegKey ${REG_ROOT} "${VERSION_TRACKING}\${APP_DISPLAY_NAME}"

SectionEnd

######################################################################

