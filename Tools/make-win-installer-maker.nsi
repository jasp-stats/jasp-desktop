# define the name of the installer
Outfile "installer-maker.exe"

;InstallDir "C:\Users\Damian\Documents\GitHub\InstallerCreator\JASP"

ShowInstDetails show

Var STR_HAYSTACK
Var STR_NEEDLE
Var STR_CONTAINS_VAR_1
Var STR_CONTAINS_VAR_2
Var STR_CONTAINS_VAR_3
Var STR_CONTAINS_VAR_4
Var STR_RETURN_VAR
 
Function StrContains
  Exch $STR_NEEDLE
  Exch 1
  Exch $STR_HAYSTACK
  ; Uncomment to debug
  ;MessageBox MB_OK 'STR_NEEDLE = $STR_NEEDLE STR_HAYSTACK = $STR_HAYSTACK '
    StrCpy $STR_RETURN_VAR ""
    StrCpy $STR_CONTAINS_VAR_1 -1
    StrLen $STR_CONTAINS_VAR_2 $STR_NEEDLE
    StrLen $STR_CONTAINS_VAR_4 $STR_HAYSTACK
    loop:
      IntOp $STR_CONTAINS_VAR_1 $STR_CONTAINS_VAR_1 + 1
      StrCpy $STR_CONTAINS_VAR_3 $STR_HAYSTACK $STR_CONTAINS_VAR_2 $STR_CONTAINS_VAR_1
      StrCmp $STR_CONTAINS_VAR_3 $STR_NEEDLE found
      StrCmp $STR_CONTAINS_VAR_1 $STR_CONTAINS_VAR_4 done
      Goto loop
    found:
      StrCpy $STR_RETURN_VAR $STR_NEEDLE
      Goto done
    done:
   Pop $STR_NEEDLE ;Prevent "invalid opcode" errors and keep the
   Exch $STR_RETURN_VAR  
FunctionEnd
 
!macro _StrContainsConstructor OUT NEEDLE HAYSTACK
  Push `${HAYSTACK}`
  Push `${NEEDLE}`
  Call StrContains
  Pop `${OUT}`
!macroend
 
!define StrContains '!insertmacro "_StrContainsConstructor"'

!macro CallFindFiles DIR ADD_DELETE CBFUNC
	Push "${DIR}"
	Push "${ADD_DELETE}"
	Push $0
	GetFunctionAddress $0 "${CBFUNC}"
	Exch $0
	Call FindFiles
!macroend
 
!macro FilterDirectory DIR BASE
	
	StrCmp "${DIR}" "${BASE}\R\Tcl" FilterDir
	StrCmp "${DIR}" "${BASE}\R\share\zoneinfo" FilterDir
	StrCmp "${DIR}" "${BASE}\R\doc" FilterDir
	
	;Begin platform specific directory filtering
	StrCpy $R8 "${DIR}" "" -4
	StrCmp $R9 "x86" 0 skip1
	StrCmp "$R8" "\x64" FilterDir
	skip1:
	
	StrCpy $R8 "${DIR}" "" -5
	StrCmp $R9 "x64" 0 skip2
	StrCmp "$R8" "\i686" FilterDir
	StrCmp "$R8" "\i386" FilterDir
	StrCmp "$R8" "\_x86" FilterDir
	skip2:
	StrCmp $R9 "x86" 0 skip3
	StrCmp "$R8" "\_x64" FilterDir
	skip3:
	;End platform specific directory filtering
	

	;Begin R directory filtering
	${StrContains} $0 "\R\" "${DIR}"
	StrCmp $0 "" notRDirectory
	
	StrCpy $R8 "${DIR}" "" -4
	StrCmp "$R8" "\doc" FilterDir

	StrCpy $R8 "${DIR}" "" -5
	StrCmp "$R8" "\html" FilterDir
	StrCmp "$R8" "\html" FilterDir
	StrCmp "$R8" "\demo" FilterDir
	StrCmp "$R8" "\help" FilterDir

	StrCpy $R8 "${DIR}" "" -6
	StrCmp "$R8" "\tests" FilterDir
	StrCmp "$R8" "\cmake" FilterDir

	notRDirectory:
	;End R directory filtering


	;add more directory filters here
	
	Goto DontFilter
	
FilterDir:
	Goto done

DontFilter:
!macroend
 
 !macro CleanPath PATH BASE_LENGTH POS
	StrCpy $4 ${PATH}
	StrCmp ${POS} 0 NoMod
	StrCpy $4 ${PATH} ${POS}
	IntOp $3 ${POS} + 5
	StrCpy $3 ${PATH} "" $3
	StrCpy $4 "$4$3"
	NoMod:
	StrCpy $6 "$4" "" "${BASE_LENGTH}"
	Push $6
!macroend
 
!macro FindDirt DIR CURRENT_POS
	StrCmp ${CURRENT_POS} 0 0 NoShift  
	StrCpy $4 ${DIR} "" -5
	StrCmp $4 "\_x64" DirShift 0
	StrCmp $4 "\_x86" DirShift 0
	StrCpy $5 0
	Goto NoShift
	
	DirShift:
	StrLen $5 "${DIR}"
	IntOp $5 $5 - 5
	NoShift:
	Push $5
!macroend
 
 Function FoundFile
	Pop $7
	
	;Begin R directory filtering
	${StrContains} $0 "\R\" "$7"
	StrCmp $0 "" notRDirectory

	# Filter file extensions
	StrCpy $0 $7 "" -2 
	StrCmp $0 ".h" done 0

	# Filter file extensions
	StrCpy $0 $7 "" -4 
	StrCmp $0 ".hpp" done 0
	StrCmp $0 ".txt" done 0
	StrCmp $0 ".doc" done 0

	# Filter file extensions
	StrCpy $0 $7 "" -5 
	StrCmp $0 ".html" done 0

	notRDirectory:


	StrCmp $R4 "Delete" 0 add
	
	!insertmacro CleanPath $7 $8 $5
	Pop $6
	
	FileWrite $9 'Delete "$$INSTDIR$6"$\r$\n'
	goto done

	add:
	#FileWrite $9 'File /nonfatal "$7"$\r$\n'
	FileWrite $9 'File "$7"$\r$\n'
	
	done:
FunctionEnd
 
 Function FindFiles
	Exch $R5 # callback function
	Exch 
	Exch $R4 # Add or Delete
	Exch 2
	Exch $R0 # directory
	Push $R1
	Push $R2
	Push $R3
 
 	Push 0 # index of \_x64 or \_x86 for trimming
	Push $R0 # first dir to search

 
	StrCpy $R3 1
	
	StrLen $8 $R0
 
nextDir:
	Pop $R0
	Pop $5
	IntOp $R3 $R3 - 1
	
	!insertmacro FilterDirectory "$R0" "$INSTDIR"
	
	StrCpy $R7 "NewDir"

	ClearErrors
	FindFirst $R1 $R2 "$R0\*.*"
	nextFile:
		StrCmp $R2 "" done
		StrCmp $R2 "." gotoNextFile
		StrCmp $R2 ".." gotoNextFile
 
		IfFileExists "$R0\$R2\*.*" isDir 0
			StrCmp $R7 "NewDir" 0 NoDirPaths
			StrCmp $R4 "Delete" NoDirPaths 0
			!insertmacro CleanPath $R0 $8 $5
			Pop $7
			FileWrite $9 'SetOutPath "$$INSTDIR$7"$\r$\n'
			StrCpy $R7 "OldDir"
			NoDirPaths:
			Push "$R0\$R2"
			Call $R5
			Goto gotoNextFile
 
		isDir:
			IntOp $R3 $R3 + 1
			!insertmacro FindDirt "$R0\$R2" $5
			Pop $5
			Push $5
			Push "$R0\$R2"

 
	gotoNextFile:
	FindNext $R1 $R2
	IfErrors 0 nextFile
 
done:
	FindClose $R1
	StrCmp $R3 0 0 nextDir
 
	Pop $R3
	Pop $R2
	Pop $R1
	Pop $R0
	Pop $R5
	Pop $R4
FunctionEnd
 
 
Function searchDirectory
Pop $R2
Pop $R5

StrCmp $R5 0 0 AlreadyDirty
!insertmacro FindDirt $R2 $R5
Pop $R5
AlreadyDirty:

FindFirst $R0 $R1 "$R2\*.*"

searchDirectory_loop:

  StrCmp $R1 "" searchDirectory_break
  StrCmp $R1 "." done
  StrCmp $R1 ".." done
  IfFileExists "$R2\$R1\*.*" 0 done
	!insertmacro FilterDirectory "$R2\$R1" "$INSTDIR"
	Push $R0
	Push $R1
	Push $R2
	Push $R5
    Call addDir
	Pop $R5
	Pop $R2
	Pop $R1
	Pop $R0
	StrCpy $R8 "$R2\$R1" "" -5
	StrCmp $R8 "\_x64" done
	StrCmp $R8 "\_x86" done
	;StrCpy $7 "$2\$1" "" "$8"
	!insertmacro CleanPath "$R2\$R1" $8 $R5
	Pop $R7
	FileWrite $9 'RmDir "$$INSTDIR$R7"$\r$\n'
  
  done:
	FindNext $R0 $R1
	Goto searchDirectory_loop
  
searchDirectory_break:
FindClose $R0
FunctionEnd


Function addDir
  ;IntOp $R8 $R8 + 1
  Push $R5
  Push "$R2\$R1"
  Call searchDirectory
FunctionEnd 
 
# open section
Section
	StrCpy $R9 "x64"
	
	Delete "includeFiles64.nsi"
	Delete "deleteFiles64.nsi"

	FileOpen $9 "includeFiles64.nsi" w
	!insertmacro CallFindFiles "$INSTDIR" "" FoundFile
	FileClose $9
	
	FileOpen $9 "deleteFiles64.nsi" w
	!insertmacro CallFindFiles "$INSTDIR" "Delete" FoundFile
	
	Push 0
	Push "$INSTDIR"
	Call searchDirectory
	FileClose $9
	

	StrCpy $R9 "x86"
	
	;MessageBox MB_OK "$_32BitSuffix"
	
	Delete "includeFiles32.nsi"
	Delete "deleteFiles32.nsi"
	
	FileOpen $9 "includeFiles32.nsi" w
	!insertmacro CallFindFiles "$INSTDIR" "" FoundFile
	FileClose $9
	
	FileOpen $9 "deleteFiles32.nsi" w
	!insertmacro CallFindFiles "$INSTDIR" "Delete" FoundFile
	
	Push 0
	Push "$INSTDIR"
	Call searchDirectory
	FileClose $9
SectionEnd