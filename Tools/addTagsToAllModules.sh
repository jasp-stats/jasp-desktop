export VERSION=0.18.1
git submodule foreach 'git tag -a "v${VERSION}" -m "version ${VERSION}"'
git submodule foreach 'git push --tags' 
