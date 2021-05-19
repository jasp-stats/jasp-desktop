
Guide to adding a module in JASP
================================

The ability to add your own module to JASP is a recently added feature (as of 0.9.3) and we will take care to avoid introducing breaking changes. The basic idea of loadable modules in JASP is that they follow the general structure of an R-package. In fact, if you have an R-package it is relatively easy to convert this to a module.

## Structure
A module folder should look as follows:

- ModuleName/
  - [inst/](#inst)
    - [Description.qml](#Descriptionqml)
    - [Upgrades.qml](#Upgradesqml)
    - [icons/](#icons)
    - [qml/](#qml)
    - [help/](#help)
  - [R/](#r)
  - [DESCRIPTION](#package-metadata)
  - [NAMESPACE](#package-metadata)

### Description.qml
The Description.qml file is the main configuration file for a JASP Module and consists of a `Description` qml root object where you can add `Package`'s and `Analysis`, `GroupTitle` and `Separator` objects to. I would suggest you take a look at the description made by the JASP team at [one our modules on GitHub](https://github.com/jasp-stats).  

It should always start with the following:
```
import QtQuick 		2.12
import JASP.Module 	1.0

Description 
{
```

#### Module Description
So the root object of your `Description.qml` is a `Description` object that has the following properties:

  | property     | description |
  |---------------|-------------|
  | `name`        | Specifies the name of the module, only necessary if you are not adding your own [DESCRIPTION](#packageMetadata) file. This name should contain only ASCII letters, numbers and dots, have at least two characters and start with a letter and not end in a dot.|
  | `title`       | The user-friendly name that is shown in the ribbonbar (top of JASP) and the modules-menu (the right-hand-side menu in JASP). |
  | `description` | A description of the module that will be shown during install. |
  | `icon`        | The filename of the icon to show in the ribbonbar. |
  | `version`     | The current version of your module, encoded as a string. This is a sequence of at least two (and usually three) non-negative integers separated by single ‘.’ or ‘-’ characters. A good example is "1.0.0", a version such as "0.01" or "0.01.0" will be handled as if it were ‘0.1-0’. It is not a decimal number, so for example "0.9" < "0.75" since 9 < 75. |
  | `author`      | Name of the author. |
  | `maintainer`  | Name and email of maintainer. An example: "John Doe \<John.Doe@Somewhere.org>". If it isn't a valid email adress **R will complain**. |
  | `website`     | Website of the author. |
  | `license`     | License under which the module is released. To have it distributed by us this should be a [free software license](https://en.wikipedia.org/wiki/Free_software_license), aka something like "GPL (>= 2)". |

  In the future most of this info will be taking straight from [DESCRIPTION](#packageMetadata).

#### Description Menu
A very important part of [Description.qml](#Description.qml) is the menu specification, as this makes it possible for a user of your module to actually run your analyses. You specify the analyses your module offers, what their titles are, which [options form](#qml) they use and which R-functions should be called to run them. Furthermore you can add separators between groups of analyses and you can add headers with icons inside the menu to make it clearer what category each group of analyses embodies. 

This menu is specified by a succession of `GroupTitle`, `Separator` and `Analysis` QML objects.

##### Analysis
The most important one and you should always have at least one of these, because otherwise your module doesn't do *anything*.
It represents, surprise surprise, one of your analyses and has the following fields:

  | fieldname  | description |
  |------------|-------------|
  | `title`    | Name of the analysis and, if `menu` is missing, the text shown in the ribbonbutton-menu. |
  | `func` 	   | Name of the main R function of an analysis, this should be part of your R-code. |
  | `qml`      | Optional: Filename of the qml file associated with an analysis, it must be located in the [qml folder](#qml). If it isn't filled `func + ".qml"` is used. |
  | `menu`     | Optional: Text shown in the menu that opens when the ribbonbutton is clicked. If it isn't entered `title` is used. |
  | `icon`     | Optional: Filename of the icon to show next to `title` in the menu. |
  

##### GroupTitle
A nice big header for your menu, can have an icon and it will be clearly visible. Useful for breaking up the list in, for instance, "Bayesian" and "Classical".
It has the following fields:

  | fieldname  | description |
  |------------|-------------|
  | `title`    | Name of the analysis and, if `menu` is missing, the text shown in the ribbonbutton-menu. |
  | `icon`     | Optional: Filename of the icon to show next to `title` in the menu, can be used by analyses and headers. |
  
##### Separator
A nice line to break your menu even better than a GroupTitle would do.
Has no properties.
  
 
<details>
	<summary>Example</summary>
  
  ```qml
import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
      title: 		"Amazing module"
      description: 	"This is a totally amazing module."
      version: 		"0.0.1"
      author: 		"yourName"
      maintainer: 	"yourName <your@name.org>"
      website: 		"yourName.org"
      license: 		"GPL (>= 2)"
    
	GroupTitle
	{
		title:    "English Analyses",
		icon:     "englishFlag.svg"
	}

	Analysis
	{
		title:	"Analysis One"
		qml:	"analysisOne.qml"
		func: 	"analysisOne"
	}

	Analysis
	{
		title: 	"Analysis Two with a very long name that might look bad in a menu"
		menu:	"Analysis Two"
		qml:	"analyseTwee.qml"
		func:	"analysisTwo"
	} 

	Separator {}

	GroupTitle
	{
		title:	"Nederlandse Analyses"
		icon:  	1"dutchFlag.svg"
	}

	Analysis
	{
		title:    "Analyse een"
		qml:      "analyseEen.qml"
		function: "analysisOne"
	}

	Analysis
	{
		title:    	"Analyse twee"
		qml:      	"analyseTwee.qml"
		function:	"analysisTwo"
		icon:    	"romanNumeralII.svg"
	}
}
  ```
  
</details>


### Upgrades.qml
The optional Upgrades.qml file contains any upgrades that must be done on older, saved, versions of your analyses. So this is only of importance after you release a second or later version of your module and it will allow for seamless loading of these older files. For more information on this, see [the manual on Upgrades.qml](jasp-upgrade-qml.md).

#### icons
The icons folder is where you should place all icons you want to show, aka one for the ribbonbar that opens the [menu](#description-menu) and possibly more for use with headers and/or analyses. Preferably this is an [svg](https://nl.wikipedia.org/wiki/Scalable_Vector_Graphics) because this scales very gracefully. You could also use [png](https://nl.wikipedia.org/wiki/Portable_network_graphics) or [jpg](https://nl.wikipedia.org/wiki/JPEG) but since these do not scale very well they could make your icon look pretty bad on some computers.  If you specify an icon somewhere in [Description.qml](#Description.qml) you should include the extension but not the path. As an example, the file `ModuleName/inst/icons/myGorgeousIcon.svg` would be specified only as `myGorgeousIcon.svg` and JASP will make sure to retrieve it from this directory.

#### qml
The qml folder is where you place your [qml](https://en.wikipedia.org/wiki/QML) files. In practice you probably want to have at least a single qml file per analysis. You can then specify which file to use per analysis in the [menu](#description-menu), the names should include the extension `.qml` as well. A detailed guide on creating QML files can be found [here](jasp-qml-guide.md). As an aside, if you want to go for creating you own reusable qml components you can store them here as well. This is described in more detail [here](jasp-qml-guide.md#advanced-usage).

#### R
In the R folder you should place your R file(s). You could add all your analyses into one file, or store separately them into separate files (recommended). You might event want to separate the reusable parts of your code further into their own file(s). The important part is that the name(s) of your main function(s) match the name(s) you specified under `function` in the analysis-entries of the [menu](#description-menu). A detailed guide on creating R analyses can be found [here](r-analyses-guide.md).

#### help
The help folder is where you place the documentation for your module. You should name the helpfile for each analysis with the exact same functionname as your analysis has. **Only all characters should be lowercase**. 
To be able to refer to another helpfile or an image in your module's  helpfile or in the `info` field of the = options in QML you can use a special string.
This string is `%HELP_FOLDER%` and when loaded it will be replaced with the path to the `help`-folder of your module.
So suppose you have an image in `module/help/img/picture.png`, to link to it you would put `"%HELP_FOLDER%/img/picture.png"` in your markdown.

#### Package Metadata
Because a JASP Module is also an R package it should contain a [DESCRIPTION](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#The-DESCRIPTION-file) file and a [NAMESPACE](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Package-namespaces) file. 
JASP uses the Imports field in a DESCRIPTION file to understand which dependencies it needs to install from [CRAN](https://cran.r-project.org/), in fact it lets R figure this out for it. It also installs the module as an actual R-package which means you must specify each of the main-analysis-functions in the NAMESPACE file to make sure JASP can see these functions. 

To actually use this shiny new module and work on it see: [development workflow](./jasp-module-workflow.md)).
