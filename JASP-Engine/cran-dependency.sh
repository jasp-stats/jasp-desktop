#!/bin/sh
PACKAGE=$1
VERSION=$2

tarball=${PACKAGE}_${VERSION}.tar.gz

tee <<QMAKE
Archive${PACKAGE}.target      = \$\$ARCHIVES/$tarball
Archive${PACKAGE}.commands = cd \$\$ARCHIVES && wget \$\$CRAN/$tarball
${PACKAGE}.target             = \$\$JASP_R_LIB_BUILD/$PACKAGE
${PACKAGE}.commands           = \$\$R_EXE CMD INSTALL --library=\$\$JASP_R_LIB_BUILD \$\$ARCHIVES/$tarball && touch -c \$\$JASP_R_LIB_BUILD/$PACKAGE
${PACKAGE}.depends            = Archive$PACKAGE
QMAKE_EXTRA_TARGETS     += $PACKAGE Archive$PACKAGE
DISTFILES               += \$\$ARCHIVES/$tarball
RPackage.depends        += $PACKAGE

QMAKE
