# PatchR.cmake patches some of the paths and values in `bin/R` and `etc/Makeconf`.
#
# A few things happen here,
#   In bin/R,
#     - Setting the R_HOME_DIR, and some of its dependent paths, e.g., R_SHARED_DIR.
#     - Setting the path to LIBR, this is going to be the path to `R.framework`
#     - On arm64 arch, replacing `/opt/R/arm64/` with `R_HOME/opt/R/arm64`
#     - On x86_64 arch, replacing `/usr/local/` with `R_HOME/usr/local`
#     - Changing the absolute symlinks of `R_HOME/fontconfig/fonts/conf.d` to relative symlinks.
#       This is one of the reason for broken notarization.
#     - Modifying the R.framework/Info.plist
#     - and removing bunch of stuff that could break the notarization
#
#
#
# WARNING:
#
#   When updating to a new R, we need to make sure that this script makes sense and it could
#   still manage to prepare a portable `R.framework`.

macro(patch_r)

  # Putting espace characters in the path
  string(
    REPLACE "/"
            "\\/"
            build_r_home_for_sed
            ${R_HOME_PATH})

  execute_process(
    WORKING_DIRECTORY ${R_HOME_PATH}/bin
    COMMAND sed -i.bak -e
            "s/R_HOME_DIR=.*/R_HOME_DIR=${build_r_home_for_sed}/g" R)

  execute_process(
    WORKING_DIRECTORY ${R_HOME_PATH}/bin
    COMMAND sed -i.bak -e
            "s/R_SHARE_DIR=.*/R_SHARE_DIR=\$\\{R_HOME_DIR\\}\\/share/g" R)

  execute_process(
    WORKING_DIRECTORY ${R_HOME_PATH}/bin
    COMMAND sed -i.bak -e
            "s/R_INCLUDE_DIR=.*/R_INCLUDE_DIR=\$\\{R_HOME_DIR\\}\\/include/g" R)

  execute_process(
    WORKING_DIRECTORY ${R_HOME_PATH}/bin
    COMMAND sed -i.bak -e "s/R_DOC_DIR=.*/R_DOC_DIR=\$\\{R_HOME_DIR\\}\\/doc/g"
            R)

  # Commenting all instances of ldpaths call
  execute_process(WORKING_DIRECTORY ${R_HOME_PATH}/bin
                  COMMAND sed -i.bak "/ldpaths/s/^/#/g" R)

  execute_process(
    WORKING_DIRECTORY ${R_HOME_PATH}/etc
    COMMAND
      sed -i.bak -e
      "s/LIBR =.*/LIBR = -F$(R_HOME)\\/..\\/..\\/..\\/..\\/ -framework R/g"
      Makeconf)

  # If this fails, I might need to have a quote aruond "$(R_HOME)",
  # or the whole line including it. I'm not sure yet.
  if(R_HOME_PATH MATCHES "arm64")
    execute_process(
      WORKING_DIRECTORY ${R_HOME_PATH}/etc
      COMMAND sed -i.bak -e
              "s/\\/opt\\/R\\/arm64/$(R_HOME)\\/opt\\/R\\/arm64/g" Makeconf)

  else()
    # On x86_64, we might need to do a bit differently since some of these packages
    # are installed in R_HOME/usr/local/ which is bizarre, but I guess we will see
    execute_process(
      WORKING_DIRECTORY ${R_HOME_PATH}/etc
      COMMAND sed -i.bak -e "s/\\/usr\\/local/$(R_HOME)\\/opt\\/local/g"
              Makeconf)
  endif()

  # -------------------------------------------------------------
  # Resolving some absolute symlinks that may cause signing issue

  file(GLOB CONF_FILES "${R_HOME_PATH}/fontconfig/fonts/conf.d/*.conf")

  foreach(FILE ${CONF_FILES})

    get_filename_component(FILE_NAME ${FILE} NAME)

    execute_process(
      WORKING_DIRECTORY ${R_HOME_PATH}/fontconfig/fonts/conf.d/
      COMMAND ln -sf ../../fontconfig/conf.avail/${FILE_NAME} ${FILE_NAME})

  endforeach()

  # -------------------------------------------------------------
  # Adding the main executable of the R.framework to the plist.Info
  # Note:
  #   - This might be problmatic since the main executable is not where Apple
  #     expect it. For now, I point it to the thing, and I hope it's good enough.
  #     If this doesn't work, we need to copy the `bin/exec/R` out, and let it
  #     sit there for show. This means that we cannot use the `./R` script in the
  #     R_HOME_PATH, and we need to call the one in the `bin` folder.
  execute_process(
    WORKING_DIRECTORY ${R_HOME_PATH}
    COMMAND
      sed -i.bak
      "s/<\\/dict>/<key>CFBundleExecutable<\\/key><string>lib\\/libR.dylib<\\/string><\\/dict>/"
      Info.plist)

  # Removing things...
  if(R_HOME_PATH MATCHES "arm64")

    execute_process(WORKING_DIRECTORY "${R_FRAMEWORK_PATH}"
                    COMMAND rm -rf R.framework/Resources.old)

  endif()

  # We are removing this because having the Libraries/ as a symlink in
  # the root of R.framework is not acceptable by Apple.
  execute_process(WORKING_DIRECTORY "${R_FRAMEWORK_PATH}"
                  COMMAND rm -rf R.framework/Libraries)

endmacro()
