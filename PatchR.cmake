# A macro for patching some of the paths and values in `bin/R` and
# `etc/Makeconf`.

macro(patch_r)

  cmake_print_variables(r_pkg_r_home)
  cmake_print_variables(R_HOME_PATH)

  # Putting espace characters in the path
  string(
    REPLACE "/"
            "\\/"
            build_r_home_for_sed
            ${R_HOME_PATH})

  execute_process(
    WORKING_DIRECTORY ${r_pkg_r_home}/bin
    COMMAND sed -i.bak -e
            "s/R_HOME_DIR=.*/R_HOME_DIR=${build_r_home_for_sed}/g" R)

  execute_process(
    WORKING_DIRECTORY ${r_pkg_r_home}/bin
    COMMAND sed -i.bak -e
            "s/R_SHARE_DIR=.*/R_SHARE_DIR=\$\\{R_HOME_DIR\\}\\/share/g" R)

  execute_process(
    WORKING_DIRECTORY ${r_pkg_r_home}/bin
    COMMAND sed -i.bak -e
            "s/R_INCLUDE_DIR=.*/R_INCLUDE_DIR=\$\\{R_HOME_DIR\\}\\/include/g" R)

  execute_process(
    WORKING_DIRECTORY ${r_pkg_r_home}/bin
    COMMAND sed -i.bak -e "s/R_DOC_DIR=.*/R_DOC_DIR=\$\\{R_HOME_DIR\\}\\/doc/g"
            R)

  execute_process(
    WORKING_DIRECTORY ${r_pkg_r_home}/etc
    COMMAND
      sed -i.bak -e
      "s/LIBR =.*/LIBR = -F$(R_HOME)\\/..\\/..\\/..\\/..\\/ -framework R/g"
      Makeconf)

  # If this fails, I might need to have a quote aruond "$(R_HOME)",
  # or the whole line including it. I'm not sure yet.
  execute_process(
    WORKING_DIRECTORY ${r_pkg_r_home}/etc
    COMMAND sed -i.bak -e "s/\\/opt\\/R\\/arm64/$(R_HOME)\\/opt\\/R\\/arm64/g"
            Makeconf)

endmacro()
