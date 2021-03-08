QT_PATH=~/Qt/5.15.2
JASP_DESKTOP_PATH=<path to gasp-desktop repo>

LANGUAGE_CODES=(nl de es pt ja tr)
MODULE_NAMES=(jaspDistributions)

export PATH=$QT_PATH/clang_64/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin;

for moduleName in ${MODULE_NAMES[@]}
do
  echo Module: ${moduleName}
  lupdate -locations none -extensions cpp,qml -recursive ${moduleName} -ts ${moduleName}/po/QML-${moduleName}.pot ; 
  msgattrib --no-obsolete --no-location ${moduleName}/po/QML-${moduleName}.pot -o ${moduleName}/po/QML-${moduleName}.pot;

  for languageCode in ${LANGUAGE_CODES[@]} 
  do 
    echo Generating language File: ${languageCode}; 
    lupdate -locations none -extensions cpp,qml -recursive ${moduleName} -ts ${moduleName}/po/QML-${languageCode}.po ; 
    msgattrib --no-obsolete --no-location ${moduleName}/po/QML-${languageCode}.po -o ${moduleName}/po/QML-${languageCode}.po ; 
    lrelease ${moduleName}/po/QML-${languageCode}.po -qm ${moduleName}/inst/qml/translations/${moduleName}-${languageCode}.qm ; 
  done
  Rscript ${JASP_DESKTOP_PATH}/Tools/translate.R ${moduleName};
done


