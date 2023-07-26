export VERSION=0.18.0
git submodule foreach 'git tag -a "v${VERSION}" -m "version ${VERSION}"'
git submodule foreach 'git push --tags' 
