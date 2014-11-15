isEmpty(PREFIX) { PREFIX = /usr/local }
JASP_R_LIBRARY = $$PREFIX/lib/JASP/R/library

CONFIG += use_jasps_own_r_binary_package

isEmpty(JASPS_OWN_R_BINARY_PACKAGE) {
    JASPS_OWN_R_BINARY_PACKAGE = $$system(cd ../R && pwd)
}

