function checkPO {
    for LANGUAGE in cs de eo es eu fr gl id ja nl pl pt_BR pt ru zh_Hans zh_Hant
    do
        PO_FILE="po/QML-${LANGUAGE}.po"
        if [ -e ${PO_FILE} ]
        then
            if ! grep X-Language ${PO_FILE} >> /dev/null 2>&1
            then
                echo "No X-Language for ${LANGUAGE}"
            fi
        fi
    done
}

export -f checkPO

git submodule foreach checkPO
