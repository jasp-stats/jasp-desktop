
isEmpty(MODULE_NAME) {
  message(You must specify MODULE_NAME to use InstallModule.pri!)
} else {
  isEmpty(MODULE_DIR) MODULE_DIR=$$PWD

  JASP_LIBRARY_DIR = $${JASP_BUILDROOT_DIR}/Modules/$${MODULE_NAME}
  include(../R_INSTALL_CMDS.pri)
  Install$${MODULE_NAME}.commands        =  rm -rf $$JASP_LIBRARY_DIR && mkdir $$JASP_LIBRARY_DIR;
  Install$${MODULE_NAME}.commands       +=  $${INSTALL_R_PKG_CMD_PREFIX}$${MODULE_DIR}/$${MODULE_NAME}$${INSTALL_R_PKG_CMD_POSTFIX}

  QMAKE_EXTRA_TARGETS += Install$${MODULE_NAME}
  POST_TARGETDEPS     += Install$${MODULE_NAME}
}
