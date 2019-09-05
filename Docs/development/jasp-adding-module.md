
Guide to adding a module in JASP
================================

The ability to add your own module to JASP is a recently added feature (as of 0.9.3) and we will take care to avoid introducing breaking changes. The basic idea of loadable modules in JASP is that they follow the general structure of an R-package. In fact, if you have an R-package it is relatively easy to convert this to a module.

Table of Contents:
- [Structure](#structure)
- [Development Process](#development-process)
- [Distributing Module](#distributing-the-module)

## Structure
A module folder should look as follows:

- ModuleName/
  - [inst/](#inst)
    - [description.json](#descriptionjson)
    - [icons/](#icons)
    - [qml/](#qml)
    - [help/](#help)
  - [R/](#r)
  - [DESCRIPTION](#package-metadata)
  - [NAMESPACE](#package-metadata)

### description.json
The description.json file is the main configuration file for a JASP Module and usually contains the following fields:

  | fieldname | description |
  |-----------|-------------|
  | `moduleDescription` | A json-object that specifies what your module is called, who made it and more. See [moduleDescription](#module-description). |
  | `requiredPackages`  | An optional field that, if specified, is an array of R packages that your analysis depends on and that are not shipped with JASP (see [the package list](https://jasp-stats.org/r-package-list)). This can take the form of a simple array of strings `["package1", "package2"]` or an array of objects `[{ "package": "package1", "version": "1.2.3"}, { "package": "packageWithoutVersion" }, ...]`. |
  | `menu`              | Specifies the icon to show in the ribbon and the analyses your module adds, see [description-menu](#description-menu). |

#### Module Description
This field should contain a json object with the following fields:

  | fieldname     | description |
  |---------------|-------------|
  | `name`        | Specifies the name of the module, only necessary if you are not adding your own [DESCRIPTION](#packageMetadata) file. This name should contain only ASCII letters, numbers and dots, have at least two characters and start with a letter and not end in a dot.|
  | `title`       | The user-friendly name that is shown in the ribbonbar (top of JASP) and the modules-menu (the right-hand-side menu in JASP). |
  | `description` | A description of the module that will be shown during install. |
  | `icon`        | The filename of the icon to show in the ribbonbar. |
  | `version`     | The current version of your module, encoded as a string. This is a sequence of at least two (and usually three) non-negative integers separated by single ‘.’ or ‘-’ characters. A good example is "1.0.0", a version such as "0.01" or "0.01.0" will be handled as if it were ‘0.1-0’. It is not a decimal number, so for example "0.9" < "0.75" since 9 < 75. |
  | `author`      | Name of the author. |
  | `maintainer`  | Name and email of maintainer. An example: "John Doe \<John.Doe@Somewhere.org>" |
  | `website`     | Website of the author. |
  | `license`     | License under which the module is released. To have it distributed by us this should be a [free software license](https://en.wikipedia.org/wiki/Free_software_license), aka something like "GPL (>= 2)". |

#### Description Menu
A very important part of [description.json](#description.json) is the menu specification, as this makes it possible for a user of your module to actually run your analyses. Here you specify the icon and title to show in the ribbonbar at the top of JASP. You also specify the analyses your module offers, what their titles are, which [options form](#qml) they use and which R-functions should be called to run them. Furthermore you can add separators between groups of analyses and you can add headers with icons inside the menu to make it clearer what category each group of analyses embodies. 

This is all specified under the field `menu` which should be an array of analyses, separators and headers which are each represented by a json-object with one or more fields. Those are: 
  
  | fieldname  | description |
  |------------|-------------|
  | `title`    | Text shown in the menu that opens when the ribbonbutton is clicked. |
  | `icon`     | Filename of the icon to show next to `title` in the menu, can be used by analyses and headers. |
  | `qml`      | Filename of the qml file associated with an analysis, it must be located in the [qml folder](#qml). |
  | `function` | Name of the main R function of an analysis, this should be part of your R-code. |
  
  When an entry has `-` (or a multple of them) as title the other fields are ignored and it will be displayed as a separator. Otherwise it will be either a header or an analysis, the only difference between them being that an analyses *always* has `qml` and `function` specified and responds to clicks in JASP.
    
 
<details>
	<summary>Example</summary>
  
  ```json
  {
    "moduleDescription" :
    {
      "title" : "Amazing module",
      "description": "This is a totally amazing module.",
      "version": 1,
      "author": "yourName",
      "maintainer": "yourName <your@name.org>",
      "website": "yourName.org",
      "license": "GPL (>= 2)"
    },

    "requiredPackages": [    
		{ "package": "package1" },
		{ "package": "package2", "version": "1.2.3" },
		{ "package": "package3" }
    ],

    "menu":
        [
          {
            "title":    "English Analyses",
            "icon":     "englishFlag.svg"
          },
          {
            "title":    "Analysis One",
            "qml":      "analysisOne.qml",
            "function": "analysisOne"
          },
          {
            "title":    "Analysis Two",
            "qml":      "analyseTwee.qml",
            "function": "analysisTwo"
          } 
          {
            "title":    "---"
          },
	        {
            "title":    "Nederlandse Analyses",
            "icon":     "dutchFlag.svg"
          },
          {
            "title":    "Analyse een",
            "qml":      "analyseEen.qml",
            "function": "analysisOne"
          },
          {
            "title":    "Analyse twee",
            "qml":      "analyseTwee.qml",
            "function": "analysisTwo",
            "icon":     "romanNumeralII.svg"
          }
        ]
  ```
  
</details>

#### icons
The icons folder is where you should place all icons you want to show, aka one for the ribbonbar that opens the [menu](#description-menu) and possibly more for use with headers and/or analyses. Preferably this is an [svg](https://nl.wikipedia.org/wiki/Scalable_Vector_Graphics) because this scales very gracefully. You could also use [png](https://nl.wikipedia.org/wiki/Portable_network_graphics) or [jpg](https://nl.wikipedia.org/wiki/JPEG) but since these do not scale very well they could make your icon look pretty bad on some computers.  If you specify an icon somewhere in [description.json](#description.json) you should include the extension but not the path. As an example, the file `ModuleName/inst/icons/myGorgeousIcon.svg` would be specified only as `myGorgeousIcon.svg` and JASP will make sure to retrieve it from this directory.

#### qml
The qml folder is where you place your [qml](https://en.wikipedia.org/wiki/QML) files. In practice you probably want to have at least a single qml file per analysis. You can then specify which file to use per analysis in the [menu](#description-menu), the names should include the extension `.qml` as well. A detailed guide on creating QML files can be found [here](jasp-qml-guide.md). As an aside, if you want to go for creating you own reusable qml components you can store them here as well. This is described in more detail [here](jasp-qml-guide.md#advanced-usage).

#### R
In the R folder you should place your R file(s). You could add all your analyses into one file, or store separately them into separate files (recommended). You might event want to separate the reusable parts of your code further into their own file(s). The important part is that the name(s) of your main function(s) match the name(s) you specified under `function` in the analysis-entries of the [menu](#description-menu). A detailed guide on creating R analyses can be found [here](r-analyses-guide.md).

#### help
The help folder is where you place the documentation for your module. You should name the helpfile for each analysis with the exact same name/title as your analysis has. Only all characters should be lowercase.

#### Package Metadata
Because a JASP Module is also, to a certain extent, an R package it can contain a [DESCRIPTION](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#The-DESCRIPTION-file) file and a [NAMESPACE](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Package-namespaces) file. 
JASP uses the Imports field in a DESCRIPTION file to understand which dependencies it needs to install from [CRAN](https://cran.r-project.org/), in fact it lets R figure this out for it. It also installs the module as an actual R-package which means you must specify each of the main-analysis-functions in the NAMESPACE file to make sure JASP can see these functions. 

If these are missing from your module this is no problem, as JASP can generate them for you based on the information in [description.json](#description.json). It does this for modules that are distributed as .tar.gz files (which is the same format as an R-package) but also for the development module (as used in [development](#development-process)). In that last scenario it will only unflinchingly create these files if they are missing. To be sure changes to description.json are reflected in them you should enable the `Preferences/Advanced/Regenerate Package metadata...` option, this will make JASP generate these file *to your development folder*. If you prefer to write your own files you are free to do so after disabling it. 

### Development Process
To start developing your own module you should first create the folder structure as shown [above](#Structure). Initially you do not need to add any of the .qml, .R or icon files, you should, however, create the description.json. The first version of this file can be quite simple (i.e., with a single analysis); you will be able to update it at any point.

If you want to start developing from an R-package that you already made you should still start by adding a rudimentary [inst/description.json](#description.json) to your sourcefolder. Be sure to turn off the `Preferences/Advanced/Regenerate Package metadata...` option of JASP as it *will* overwrite your handcrafted [DESCRIPTION and NAMESPACE files](#package-metadata).

Before creating any of the other files you should now add the module folder in JASP and then install the module.

##### Steps to add the module folder as a development module in JASP:
1. Open JASP
2. Click on the menu in the top left corner
3. Navigate to 'Preferences'
4. Navigate to 'Advanced'
5. Place a checkmark before 'Developer mode'
6. Working from R-source-package? Disable 'Regenerate Package metadata'
7. Browse to the module source folder, aka a folder with the same name as your module and that contains `inst/description.json`.

##### Steps to install the module in JASP:
1. Open JASP
2. Click on the plus symbol in the top right corner
3. Click on Install Developer Module

The advantage of installing the module is that all changes you make from this point onwards are (almost) instantly reflected in JASP. If you add a checkbox in your .qml file, this checkbox will also appear on the analysis input panel of your module. Similarly, if you change the title of your module in the .json file this will immediately change on the ribbon. As such JASP becomes a development tool, making it much easier to check your changes are correct as you make them. It might take a little while to see changes in R though, because JASP needs to rebuild the package and install it internally. So try to be patient with it ;)

At this point you can start adding the various files the module requires. It is advisable to start with the .qml interface file before adding the analysis in R.

##### Distributing the module
If you feel your module is  ready to be distributed you should package it as an R-source-package. This can be done the easy way if you developed your module in a GitHub repository or the hard way if not.

###### Easy way through GitHub
Commit and push your changes and make an annotated tag. Suppose you are releasing v1.0 of your jasp module, the following steps will make sure GitHub generates the module.tar.gz for you and will make it available online immediately:
```bash
cd /to/your/module/repository/directory
git tag -a v1.0 #It will open an editor where you can add some information on your release
git push -u origin v1.0
```
If you now go to your GitHub repository you can select `Releases` and download the generated .tar.gz, see the following, ahum, visual guide for more details:
![Visual guide to finding a generated .tar.gz on GitHub](/Docs/development/img/FindingModuleTarGz.png)

###### Hard way
On linux and MacOS this is not so bad: simply open up a terminal and go to the directory containing your module and enter the following: "`tar -czf <ModuleName>.tar.gz <ModuleName>`". 

On Windows this is a bit more complicated but can be done through [7zip](https://www.7-zip.org/), first your select you folder `<ModuleName>` and compress it to a `.tar` file and then you select that file and compress it to a `.gz` or "gzip" file leaving you with `<ModuleName>.tar.gz`. 

As you can see this implies that the folder containing your module-files has the same name as your module (aka what is specified in the field `name` in [description.json](#description.json) or in the `Package` field of [DESCRIPTION](#package-metadata).

