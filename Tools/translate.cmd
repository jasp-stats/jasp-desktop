rem Create the R-nl language .mo file

rem %1 is Rscript location
rem %2 is get gettext location (stripped "")
rem %3 Tools location
rem %4 R source folder

set PATH=%PATH%;%~2
%1\Rscript %3\translate.R %4
