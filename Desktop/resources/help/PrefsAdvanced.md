
Advanced Preferences
=========

With the advanced parameters in JASP you can specify the following options:
(All these settings will remain after restarting JASP.)

## Modules options

### Remember enabled modules
If you've enabled this option then JASP will remember which modules are activated and make sure they remain that way even when you close JASP. So supposing `Summary Statistics` was enabled and JASP closes then after reopening JASP it will be enabled immediately.

### Developer mode

If you enable this you see a few extra options appear. One is "Generate markdown files for help", which will ignore any markdown helpfiles for an analysis and instead will only show generated markdown from each `info` field on each qml item and jaspObject. Another option is the "Module library URL" field, which allows you to change the URL of the JASP module library web application from [https://module-library.jasp-stats.org/](https://module-library.jasp-stats.org/) to a locally running instance.

#### Development module
Besides that it will show "Development module" where you can load an R-package installed/restored with `renv`. For this you need to know the name of your module (for instance: `jaspAnova`) and the R-library or `.libPath` that `renv` created for you. 

To use `renv` to install your package you first `git clone` or `git checkout` the code you want.
An example using `jaspAnova`:
```bash
# in a terminal:
git clone https://github.com/jasp-stats/jaspAnova
cd jaspAnova
R
```
If you don't have `renv` installed already you can get it by running `install.packages('renv')`.
Thus we are in the `jaspAnova` folder we just created and then in R:
```R

renv::restore(clean = TRUE)     # Project library is filled with renv.lock dependencies
renv::install('.')              # Install local pkg to project library

message("R Project library for developer mode:\n", .libPaths()[1])
```

This will print the project library you need for running your module. 
You make sure to copy that to the required preference field and do the same for the module name.

As a sidenote: running the first `renv` command for a project library might actually trigger `renv::init`, which will ask the user to agree to managing some files for the user. Just answer `Y` there.

#### Temporary project library
Often the default library is also the user's normal R library, for instance something like "C:/Users/A_User/AppData/Local/R/win-library/4.5". Restoring with `renv` there can mess up the entire normal library then, as it removes and adds packages based on the development module being installed. While this is very fast and often avoids reinstalling stuff it might be undesirable.
A simple way to avoid this is to tell `renv` to install to a temporary folder, keeping your usual library as-is.
```R
.libPaths(tempdir())
# restore, install and print project library
```
On Windows you would then get something like: `C:/Users/A_User/AppData/Local/Temp/RtmpYN3aqC`

#### Dedicated project library
You could of course also decide to make a dedicated project library somewhere and use that instead of `tempdir()`, or if the temporary directory it creates isn't accessible for JASP. 
As an example, you can create a directory somewhere and pass that to `renv`.
```R
.libPaths("~/jasp-renv-package-library")
```
This will load `jasp-renv-package-library` from your homefolder (which would make it `/home/username/jasp-renv-package-library` on macos/linux or `c:\Users\username\jasp-renv-package-library` on windows).
You then do the above `renv::restore` and `renv::install`.

#### Installing extra packages
Until we finish syntaxmode for JASP, which is now in beta, you might want to use `jaspTools` to run analyses in R. You can just run `renv::install('jaspTools') and it will be installed to your project library. The same goes for any other package(s) you might need.

If you've installed other packages while developing you might want to add them to the `renv.lock` file, is easy. However you probably don't want to add all the packages loaded to the `renv.lock` file, so we run it while excluding some packages:
```
renv::snapshot(exclude=c('jaspTools'))
```
If there are other packages you do not want listed as a dependency you can simply add them to the `exclude` list, but `jaspTools` should definitely not be a dependency of a jaspModule.

#### renv guide
To understand more about `renv` you should consider reading their [getting started guide](https://rstudio.github.io/renv/articles/renv.html). It will explain what a project library is, and why this system is helpful in the first place.

### Configuration file options

Here you can select a path to a `toml` configuration file for JASP.
This allows you to preload certain modules on a system, or set some default options for particular analyses. This file can be located on a users computer or come from a remote URL for workplace deployment.

<details>
<summary>Creating you own configuration file</summary>

To make your own configuration file you create a `toml` file somewhere, for instance `my-first-jaspconfig.toml`. An example:
```toml
Format = "0.1.0"
JASPVersion = "0.19.3"

EnabledModules = ["jaspAudit",]
OverrideCommon = ["jaspAudit", "jaspMachineLearning", "jaspQualityControl"]

[Constants]
rain = true

[Modules.jaspAudit.Constants]
high = 90
medium = 50
low = 40

[Modules.jaspAudit.Analyses.auditClassicalWorkflow.Constants]
high = 77 #these will take precedent over the Module wide and JASP global constants
low = 42

[Modules.jaspAudit.Analyses.auditClassicalWorkflow.Options]
min_precision_test = {Value = true, Lock = true} #comment
materiality_test = false #comment
min_precision_rel_val = 0.14
materiality_rel_val = {Value = 0.02, Lock= true}
```

These constants can be used by module developers to change some settings for particular users and incorporate that smoothly into their analysisforms (qml files).
They can be accessed as follows:
```qml
x = form.getConstant("constant", <default value if not defined>)
```

</details>

### Github personal access tokens
When a dynamic module is installed JASP uses R internally to get all dependencies and for this it requests info from https://github.com and often this goes well.
They do have a rate limiter there however, see https://docs.github.com/en/rest/overview/resources-in-the-rest-api#rate-limiting and that can cause the module installation to fail.

To work around this we have added some options to manage a so called personal access token, and it is then passed on to R through an environment variable called `GITHUB_PAT`. This is also very useful for doing stuff in RStudio.

We added a default one that is available for R inside of JASP, but this is shared with all JASP users and thus might (at some point) become oversaturated. In that case you might want to add your own.

This can be done by generating a new token for your github account at https://github.com/settings/tokens/new and then either copying the resulting code into the settings here in JASP or through your OS. If set in your OS, you might have done this for R already, JASP will automatically use it if set use "the default" value.

Please *DO NOT* give this personal access token **any permissions** at all, because it really isn't necessary for the way it is used in JASP.

Otherwise you can set it specifically in JASP by unchecking "Use default PAT" and then copying your token-code into the custom GITHUB_PAT textbox, this is probably the easiest if you do not know what an environment variable is.

## Logging options

### Log to file
When you check this JASP will start logging many of the actions it performs to logfiles. 
Logging is especially useful when you are developing your own module, or run into a problem and wish to reach out to the development team.
The logs might help us give insight in the nature of your problem. Note that you might need to restart JASP for the logging process to start.
The number in the input field "Max logfiles to keep" defines how many logfiles will be kept at maximum to conserve diskspace. Any extra, older, logfiles will be removed.
The files can be viewed by pressing "Show logs".


## Engine options
JASP uses R in the background to run all the analyses. Here you can change how many JASP is allowed to start at the most. When deciding how many maximum engines you might want it is helpful to keep in mind that right now each module is ran by only a single engine at the moment. So having 10 analyses from the ANOVA module will not go any faster by allowing maximally 10 engines. While running 10 analyses from 10 modules will go *way* faster.

### Sandbox
On Windows you will also see the "Sandbox engine" checkbox. When enabled this will isolate the engine process in such a way that none of the code we run in R is able to do anything with the rest of your system. No networking nor access to files on the system.
This is good, because we rely on a lot of opensource third party R packages from CRAN and Github. And while we have not heard of any supplychain attacks via the R package ecosystem we think it is better to avoid such problems as much as possible.

When developing R modules (and thus enabling "Developer mode") this will be disabled by default because otherwise the engine won't be able to load your development module.

On macOs the operating system already implements at least a file access sandbox for applications and will ask for permission before accessing files outside it's "JASP.app". 

The same goes for the flatpak distribution of JASP on Linux, which in essence is a sandbox, which mediates access through file dialogs and the like. Here however we ask for home-directory read-access so that we can get normal user readable paths to recent files.
This is entirely disableable by changing JASP's configuration per `flatpak override org.jaspstats.JASP --nofilesystem=~`.


### Show engines
This opens a window the developers use to keep an eye on what each engine is up top.
It could be interesting to see what engines are busy and with what, although the level isn't very detailed. It does however give you the option to manually kill and engine by right-clicking it. 
This can be helpful if it stopped responding during some particularly heavy yet unnecessary calculation.
