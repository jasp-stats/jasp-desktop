list(APPEND CMAKE_MESSAGE_CONTEXT Modules)

if(FLATPAK_USED)


else()


configure_file(${PROJECT_SOURCE_DIR}/Modules/modules-settings.json
               ${MODULES_BINARY_PATH}/modules-settings.json)

#configure modules install script
configure_file(${PROJECT_SOURCE_DIR}/Modules/install-modules.R.in
		           ${SCRIPT_DIRECTORY}/install-modules.R @ONLY)

#create modules install target
add_custom_target(
  Modules
  USES_TERMINAL
  WORKING_DIRECTORY ${R_HOME_PATH}
  DEPENDS ${JASP_MODULE_BUNDLE_MANAGER_LIBRARY}/jaspModuleBundleManager
  DEPENDS ${SCRIPT_DIRECTORY}/install-modules.R
  COMMAND ${CMAKE_COMMAND}  -E env "JASP_R_HOME=${R_HOME_PATH}" ${R_EXECUTABLE} --slave --no-restore --no-save
        --file=${SCRIPT_DIRECTORY}/install-modules.R
  BYPRODUCTS  ${MODULES_BINARY_PATH}/bundles-downloaded.txt
  BYPRODUCTS  ${MODULES_BINARY_PATH}/bundles-installed.txt
  COMMENT "------ Installing Modules"
)

endif()
list(POP_BACK CMAKE_MESSAGE_CONTEXT)
