# A macro for patching some of the paths and values in `bin/R` and
# `etc/Makeconf`.

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
  execute_process(
    WORKING_DIRECTORY ${R_HOME_PATH}/etc
    COMMAND sed -i.bak -e "s/\\/opt\\/R\\/arm64/$(R_HOME)\\/opt\\/R\\/arm64/g"
            Makeconf)

  # execute_process(
  #   WORKING_DIRECTORY ${R_HOME_PATH}/etc
  #   COMMAND
  #     sed -i.bak -e
  #     "s/FC = \$\(R_HOME\)/opt/R/arm64/bin/gfortran -mtune=native/gfortran -mtune=native/g"
  #     Makeconf)

endmacro()
