export VERSION=0.18.0

function setVersion {
    BRANCH=`git remote show origin | sed -n "/HEAD branch/s/.*: //p"`
    echo $sm_path
    echo $BRANCH
    sed -i '' -e "s/^Version:.*$/Version: ${VERSION}/g" ./DESCRIPTION
    sed -i '' -e 's/^\tversion.*$/\tversion\t\t\t: "'${VERSION}'"/g' ./inst/Description.qml || true
    git add ./DESCRIPTION
    git add ./inst/Description.qml || true
    git commit --message "Update version to ${VERSION}"
    git push origin HEAD:${BRANCH}
}

export -f setVersion

git submodule update --remote
git submodule foreach setVersion

# To check it: git submodule update --remote
# Then:
# git submodule foreach 'head -4 DESCRIPTION | tail +4'
# git submodule foreach 'head -15 inst/Description.qml | tail +5'
