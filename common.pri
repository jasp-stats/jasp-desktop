isEmpty(PREFIX) { PREFIX = /usr/local }
JASP_R_LIBRARY = $$PREFIX/lib/JASP/R/library

macx: isEmpty(BOOST) {
    BOOST = $$system(cd ../boost_1_54_0 && pwd)
}
!isEmpty(BOOST) {
    INCLUDEPATH += $$BOOST
}

#CONFIG += use_jasps_own_r_binary_package

use_jasps_own_r_binary_package:isEmpty(JASPS_OWN_R_BINARY_PACKAGE) {
    JASPS_OWN_R_BINARY_PACKAGE = $$system(cd ../R && pwd)
}

CONFIG += c++11
