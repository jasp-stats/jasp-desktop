
Guide to adding a module in JASP
================================

This documentation is rudimentary and subject to change. The ability to add your own module to JASP is under active development. 

Table of Contents:
- [Structure](#structure)
  * [description.json](#descriptionjson)
  * [icons](#icons)
  * [qml](#qml)
  * [R](#r)
- [Development Process](#development-process)

### Structure
A module folder should look as follows:
- moduleName 
  - description.json
  - icons
    - \<moduleName>.svg
  - qml
    - \<analysisName>.qml
  - R
    - \<analysisName>.R

#### description.json
The description.json file should contain the following fields:

- `moduleDescription`
  - `title`: text that is shown in the panel where modules can be installed
  - `description`: text that will be shown once the module is shipped with JASP and the user can browse the different modules
  - `title`: text that is shown on the ribbon below the icon
  - `icon`: name of the icon found in the icons folder
  - `version`: integer specifying the current version of your module
  - `author`: name of the author
  - `maintainer`: name of the maintainer
  - `website`: website of the author
  - `license`: license under which the module is released (standard should be "GPL (>= 2)")

- `requiredPackages`: array of of R package names that your analysis depends on and that are not shipped with JASP (see [the package list](https://jasp-stats.org/r-package-list))

- `menu`
  - `title`: text that identifies an analysis when the module is clicked on the ribbon
  - `function`: name of the main R function of an analysis located in one of the R files in the R folder
  - `qml`: name of the qml file associated with an analysis that can be found in the qml folder. If no qml is specified, then the function name is taken as qml file name.
  - `type`: per default the type of the menu item is "analysis". You can specify also "separator" (this draws a horizontal line in the menu) or a "groupTitle" (the title is displayed with bold style). In the later case, you can also specify an icon.

 You can add multiple analyses and they will all appear below the ribbon icon of your module. Each analysis should have its own `title`, `qml` file and R `function`.
 
<details>
	<summary>Example</summary>
  
  ```json
  {
    "moduleDescription" :
    {
      "title" : "Amazing module",
      "description": "This is a totally amazing module.",
      "icon": "module.svg",
      "version": 1,
      "author": "yourName",
      "maintainer": "yourName <your@name.org>",
      "website": "yourName.org",
      "license": "GPL (>= 2)"
    },

    "requiredPackages": [    
		{ "package": "package1" },
		{ "package": "package2" },
		{ "package": "package3" }
    ],

    "menu":
    [
      {
        "title": "Analysis Module",
        "qml": "analysisName.qml",
        "function": "analysisName"
      },
      {
        "title": "Analysis Module 2",
        "qml": "analysisName2.qml",
        "function": "analysisName2"
      }
    ]
  }
  ```
  
</details>

#### icons
In the icons folder you should place a .svg that will be shown on the ribbon. The name of this file should match the name specified under `icon` in description.json.

#### qml
In the qml folder you should place one .qml file per analysis. The names of these files should match the names you specify under `qml` in description.json. A detailed guide on creating QML files can be found [here](jasp-qml-guide.md).

#### R
In the R folder you should place your R file(s). You could add all your analyses into one file, or store separately them into separate files (recommended). The important part is that the name of your main function matches the name you specified under `function` in description.json. A detailed guide on creating R analyses can be found [here](r-analyses-guide.md).

### Development Process
To start developing your own module you should first create the folder structure as shown above. You do not need to add any of the .qml, .R or .svg files, you should, however, create the description.json. The first version of this file can be quite simple (i.e., with a single analysis); you will be able to update it at any point.

Before creating any of the other files you should now add the module folder in JASP and then install the module.

Steps to add the module folder in JASP:
1. Open JASP
2. Click on the menu in the top left corner
3. Navigate to 'Preferences'
4. Navigate to 'Advanced'
5. Place a checkmark before 'Developer mode'
6. Enter the path where the description.json file will be found

Steps to install the module in JASP:
1. Open JASP
2. Click on the plus symbol in the top right corner
3. Click on Install Developer Module

The advantage of installing the module is that all changes you make from this point onwards are instantly reflected in JASP. If you add a checkbox in your .qml file, this checkbox will also appear on the analysis input panel of your module. Similarly, if you change the title of your module in the .json file this will immediately change on the ribbon. As such JASP becomes a development tool, making it much easier to check your changes are correct as you make them. 

At this point you can start adding the various files the module requires. It is advisable to start with the .qml interface file before adding the analysis in R.
