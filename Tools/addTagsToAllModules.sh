export VERSION=0.17.3
git submodule foreach 'git tag -a "v${VERSION}" -m "version ${VERSION}"'
git submodule foreach 'git push --tags' 
