## Note to developers of jasptools
These are some short musings on the internal workings of jasptools.  

jasptools calls the function `run` in `common.R` instead of directly calling the analysis itself.  
The advantage of this method is that changes to `run` in `common.R` are immediately incorporated in jasptools.  
The downside of this approach is that it needs to convert the results from JSON (and thereby loses attributes) as well as use an alternative method to obtain the state file (see the next section).

#### Globals
jasptools uses three different mechanisms to share variables between inner functions and to the environment outside jasptools.
1. `.internal`: environment that holds internal variables; these are used to pass information between functions.
The state entry is special, because it interfaces directly with the JASP code. Rather than storing and then loading the state, it is passed to jasptools, which is much faster.
2. `.pkgOptions`: environment that holds variables to be changed by users; mainly paths to resources.
3. global variables: the rbridge in JASP defines certain global variables that JASP assumes are always globally accessible and these need to be matched in jasptools. They include `perform`, `.ppi` and `.baseCitation` (the global functions on the other hand are defined in the jasptools file rbridge.R). Global variables are set every time `run` is called through `.setRCPPMasks`.

#### Handling of S3 methods
Currently it is necessary to export the S3 methods used for generic functions in JASP (e.g., those defined in `common.R`).  
The reason for this is that they are not found on the search path when defined in an environment within jasptools (see `utils::methods`) and consequently are not registered.  
The S3 methods are briefly exported during runtime to the .GlobalEnv with `.exportS3Methods` before they are again destroyed.

#### Changing package resources
jasp-desktop follows a fixed structure, meaning that in every development environment the resources are in the same place.  
Once jasptools verifies that it is located within /Tools/ it converts the relative paths to the resources to absolute paths.  
Now, if these resources are changed, jasptools will need to be adjusted. Firstly, `zzz.R` needs to be modified where it states `# set locations of all required resources (json, analyses, html, packages)` and secondly `.pkgOptions` in `pkg-settings.R` must reflect the same changes.

#### Changing to TOML
At the moment jasptools only supports JSON format for the description files.  
Should we decide to change this in the future, then the functions that interface with the description file must be adapted.  
These functions can be found in `options.R` (`.analysisOptionsFromFile`), `main.R` (`run`) and in `utils.R` (`.getJSON`).  
It is not a problem that the code that is send to `view` still uses JSON as this is unlikely to change.