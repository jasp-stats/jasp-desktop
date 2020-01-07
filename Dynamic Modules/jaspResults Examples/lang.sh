#!/bin/sh
/Users/fransmeerhoff/Qt/5.13.1/clang_64/bin/lupdate -extensions cpp,qml -recursive . -ts ./inst/qml/po/module.po
/Users/fransmeerhoff/Qt/5.13.1/clang_64/bin/lupdate -extensions cpp,qml -recursive . -ts ./inst/qml/po/module_nl.po
/Users/fransmeerhoff/Qt/5.13.1/clang_64/bin/lrelease ./inst/qml/po/module_nl.po -qm ./inst/qml/qm/module_nl.qm
