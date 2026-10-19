# Generate allhelp.h / allhelp.cpp from every help file in resources/help.
# For each <name>.md we generate a translatable string holding its
# content, and expose it on the AllHelp class through a Q_PROPERTY + member.
# (Done before the source/header globbing below so the generated files get picked up.)
# Note: file(GLOB) is evaluated at configure time, so adding a help file requires re-running CMake.
list(APPEND CMAKE_MESSAGE_CONTEXT HelpGenerator)

function(generate_var_from_helpfile input_file class_name var_name)
    file(READ ${input_file} content)
    set(before_delim "QString ${class_name}::${var_name}()\n{\n\t return tr(R\"for_c++_include")
    set(after_delim "for_c++_include\");\n}")
    set(content "${before_delim}(\n${content})${after_delim}")
    # Quote the expansion: unquoted ${content} is split on ';' (CMake's list separator),
    # and string(APPEND) would then drop those semicolons — silently deleting every ';'
    # in the help text (e.g. turning "&#8984;" into "&#8984"). Quoting keeps it verbatim.
    string(APPEND ALLHELP_VARS "${content}\n")

    # A function() has its own variable scope, so the append above only touches a local
    # copy. Push the accumulated value back to the caller so it survives across iterations.
    set(ALLHELP_VARS "${ALLHELP_VARS}" PARENT_SCOPE)
endfunction()


file(GLOB ALLHELP_FILES "${MD_FOLDER}/*.md")

set(ALLHELP_PROPERTIES      "")
set(ALLHELP_READMETHODS     "")
set(ALLHELP_VARS            "")

foreach(help_file ${ALLHELP_FILES})
  get_filename_component(help_name ${help_file} NAME_WE)
  generate_var_from_helpfile(
    "${help_file}"
    "AllHelp"
    "${help_name}")
  string(APPEND ALLHELP_PROPERTIES "Q_PROPERTY(QString\t${help_name}\tREAD ${help_name}\tNOTIFY helpChanged)\n\t")
  string(APPEND ALLHELP_READMETHODS "QString ${help_name}();\n\t")
endforeach()

configure_file(${ALLHELP_FOLDER}/allhelp.h.in
               ${ALLHELP_FOLDER}/allhelp.h)
configure_file(${ALLHELP_FOLDER}/allhelp.cpp.in
               ${ALLHELP_FOLDER}/allhelp.cpp)
message(STATUS "allhelp.h and allhelp.cpp are successfully generated...")




list(POP_BACK CMAKE_MESSAGE_CONTEXT)
