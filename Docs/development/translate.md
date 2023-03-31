
# Guide to Create a Version of JASP in Your Own Language
## Summary

<br>Since JASP version 0.12 it is possible to make JASP available in different languages. The following sections describe the different steps needed to create an international version of JASP. The translation of JASP depends on so-called portable object, '.po' files. These .po files contain all the strings in JASP that need translating. For translators, section 5 is most important. The other sections are written for developers, and provide an overview of the changes in the source code necessary for internationalization. <br>

## Contents:

* [1. Introduction.](#1-Introduction)
* [2. Inventory: Identifying all the source file types that contain strings that are shown to the user.](#2-Inventory-Identifying-all-the-source-file-types-that-contain-strings-that-are-shown-to-the-user)
* [3. The translation files used for internationalization.](#3-Definitions-and-terms)
* [4. Utilities needed for translations.](#4-Utilities-needed-for-translations)
* [5. Tasks for a translator of JASP.](#5-Tasks-for-a-translator-of-JASP)
* [6. References.](#6-References)

### 1. Introduction.

To aid the internationalization of JASP, the source files were heavily modified. The JASP core code consists of two main functional parts. The first is Interface, which defines the user interface such as windows and file menus. Interface is mainly written in C++ and QML. The second part is Engine, which executes the R-code from each analysis. Engine is mainly written in R (and C++, but this code does not produce any messages for the users).  Internationalization requires us to go through the following three steps:

1. An inventory was made of all the types of source files in JASP that contain messages to the user. These file types are described in the [Inventory: Identifying all the source file types that contain strings that are shown to the user](#2-Preparing-different-type-of-source-files) section.
2. From this inventory all strings are extracted and .po files were generated. How all the .po files are structured and how they are created out of the source files is described in the 'How to generate .po files' section.
3. The .po files can be thought of as dictionaries which are able to translate the messages from language to another in JASP at runtime.

### 2. Inventory: Identifying all the source file types that contain strings that are shown to the user.
This section describes which files types needed to be adapted such that the translation tools, described below, can extract the appropriate messages to the user. All the actions described here have already been carried out in the latest JASP 0.12 version. But if some translation is still missing, then the source code must be changed according to the procedure described here. Of course this also applies to new code.

1. Qml Files

	* a. All literal strings such as a label, or text field should be embedded in the qsTr function, that is, _qsTr_ ("Log to file ")
	* b. Use %x to Insert parameters into a string. As each language differs in structure, we advice to not create sentences by concatenating words and data. Instead, use % to insert parameters into strings. For example, the following snippet has a string with two number parameters %1 and %2. These parameters are inserted with the .arg() function. Text{ text: qsTr(“File %1 of %2).arg(counter).arg(total) } %1 refers to the first parameter and %2 refers to the second parameter so this code produces output like: "File 2 of 3".

2. C++ files

	*  a. In the .cpp files you should use the tr() function for all literal text, e.g.,

	errorMsg = tr("Refreshing the analysis may change the results.");

	*  b. Similar as mentioned above use the % character for parameters in a string for example:

	errorMsg = tr("Cannot find a source %1 for VariableList %2").arg(dropKey).arg(listView->name())

3. R-files

	*  a. All literal strings such as titles and messages must be embedded in the gettext() function, e.g, title=gettext("Hypothesis")
	*  b.  A single % character in a gettext must be transformed within a gettextf with a double %%. Please let us know if you have a better solution.
	*  c. All paste and paste0 functions must be replaced by the gettext or the gettextf functions. For example:

       overtitle = paste0(100 * options$confidenceIntervalInterval, "% CI for Proportion"))

			 should be coded as:

       overtitle = gettextf("%i%% CI for Proportion", 100 * options$confidenceIntervalInterval))

       But it is sometimes also possible to combine the two approaches:

       Paste0(gettext(“This is ok”), gettext(“Not very useful but possible”))

	*  d. Convert sprintf() into gettextf() directly.
	*  e. Furthermore, immediately after % the parameters 1$ to 99$ can be used to refer to numbered arguments: this allows arguments to be referenced out of order, and is mainly intended for translators of error messages. If this is done it is best if all formats are numbered: if not the unnumbered ones process the arguments in order in which they appear in the code. See the example. This notation allows arguments to be used more than once, in which case they must be used as the same type (integer, double or character). For example:

       message <- sprintf("Some entries of %s were not understood. These are now grouped under '%s'.", options[["colorNodesBy"]], undefGroup)

       should be coded as:

       message <- gettextf("Some entries of %1$s were not understood. These are now grouped under '%2$s'.", options[["colorNodesBy"]],undefGroup)

	*  f. Be careful with expressions, they have to be considered one by one. For instance,

       xName<- expression(paste("Population proportion", ~theta))

       should be coded as:

       xName <- bquote(paste(.(gettext("Population proportion")), ~theta))



### 3. Definitions and terms.
Once all messages are encapsulated by functions such as _qsTr_, _tr_, or _gettextf_, the messages can be extracted and collected into translation files. Here we provide a short description of these files.

* .po files:<br>A .po or Portable Object file is a text-based editable file. These types of files are commonly  used in software development. The .po file may be referenced by Java programs, GNU gettext, or other software programs as a property file. These files are saved in a human-readable format and they can be viewed in a text editor by engineers and translators. <br>The main entries in a .po file are 'msgid' (i.e., message identification) and 'msgstr' (i.e., message translation). The first entry contains the untranslated message, the second the translated string. For instance, for a Chinese translation file:<br><br>msgid "Free:"<br>
msgstr "免費"<br>

* .pot files: A .pot file or Portable Object Template file, is the file that you get when you extract texts from the application. Normally, this is the basic untranslated file send to translators. It serves as a basis for the translation of other language specific .po files. PO and POT files are essentially the same. The difference is in the intended use. This is why the two files have different extensions.
* .mo files:<br>MO or Machine Object is a binary data file that contains object data referenced by the program. It is typically used to translate program code, and may be loaded or imported into the GNU gettext program. In our case these files are generated from the R code by means of the GNU gettext tools.
* .qm files: qm or a Qt Multi language file is a compiled Qt translation file. In many ways it is similar to gettext, in that it uses a hashing table to lookup the translated text. In older version they seemingly only stored the hash and the translation.

### 4. Utilities needed for translations.

##### Qt linguistic tools from Qt 5.14.2 installation

Qt provides two specific command line tools for its internationalization.<br>
- lupdate: The lupdate command line tool finds the translatable strings in the specified source, header and Qt Designer interface files, and produces or updates .po translation files.<br>
- lrelease : The lrelease command line tool produces QM files from the .po files. The QM file format is a compact binary format that is used by the localized application. It provides extremely fast lookups for translations.<br>
These two tools are installed once Qt is installed. <br>Download the Open Source version from (https://www.qt.io/download). Please see our documentation for more info on installing Qt https://github.com/jasp-stats/jasp-desktop/blob/stable/Docs/development/jasp-building-guide.md

##### R 3.6.1
R is needed to generate .mo files.JASP 0.12 is using version 3.6.1 but any later version will do also. Installation is straightforward. You can download R from https://cran.r-project.org/

##### GNU gettext tool set
The GNU gettext utilities are a set of tools that provides a framework within which other free packages may produce multi-lingual messages. This tool set is used under the hood by R to produce .mo files.

Installations on MacOS: <br>
\> brew install gettext<br>\> brew link --force gettext<br><br>
On Windows:<br>
Download Windows binaries from: http://gnuwin32.sourceforge.net/packages/gettext.htm<br>
Unzip gettext-0.14.4-bin.zip and its location needs to be added to the PATH environment.

##### Structuring the R code (especially for JASP and jaspGraphs package).<br>
This subject is intended for developers who wish to add new translatable R-packages to JASP. Each R package/dynamic module needs to be structured as follows:

* DESCRIPTION
* NAMESPACE
* nst
* po*
* R
* po**

po* and po** can be automatically generated by tools::update_pkg_po("~/path/to/root/of/package"). This function requires the following tools from the GNU: xgettext, msgmerge, msgfmt, msginit and msgconv (see also https://developer.r-project.org/Translations30.html). The installation of the GNU tool set is needed to setup this structure.

Note that R does not set the meta data of the .pot and .po files correctly. You have to do this yourself. The R package poio can correct some of the meta data for you. It might be more user-friendly than using the functions from the tools described below.

To initiate a translation for a module:

2.	Initialize the po folders with tools::update_pkg_po("~/path/to/root/of/package"); this also creates a package .pot file in po\*\*; this file is used later to update the each translation files.
3.	Create an empty translation template by running tools::xgettext2pot("~/path/to/root/of/package", "~/path/to/root/of/package/R-<language>.po")
4.	Translate the strings in the language specific .po file.
5.	Run tools::update_pkg_po("~/path/to/root/of/package"), this ensures that all existing .po files in po** are updated by the .pot file, compiled and installed in po*.
6.	Ensure that the locale of R is set correctly when the R starts (see https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Localization-of-messages). Or specify this during run time throughSys.setenv(LANG = "<language>")


### 5. Tasks for a translator of JASP.
As described above, most user interfaces, messages and errors shown by JASP are contained in (at the moment three) .po files. At the time of writing, help files and other text files, such as some of the json files, are not included in the translation package, but the are planned to be translated in a future release. In the next sections 'xx' stands for the unique two-letter code of the language (e.g., nl for Dutch), see https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes for the complete list.

#### *Location of the .po files*
Translaters familiar with github:<br> could clone or fork the jasp-desktop repository and will find then<br>
..../jasp-desktop/Desktop/po/jasp.po (and jasp\_nl.po for the Dutch version) Copy this jasp.po file, rename it to jasp\_xx.po, and adjust the header.<br>
..../jasp-desktop/Engine/JASP/po/R-JASP.pot. Copy this R-JASP.pot file, rename the copy to R-xx.po and adjust the header.<br>
..../jasp-desktop/Engine/jaspGraphs/po/R-jaspGraphs.pot. Copy this R-jaspGraphs.pot file, rename the copy to R-xx.po, and adjust the header.

Alternatively, a prepared zip file with the jasp-desktop structure can downloaded:<br>
This compressed zip can be downloaded from:  https://static.jasp-stats.org/PO-Files-0.12.1-S.zip<br>
Unzipping this file gives you a jasp-desktop folder with only the necessary files to translate and generate the binary translation files, which you can test in you own build of JASP.<br>

Or you can just use the po files zip:<br>
This zip file only contains the .po files that must be translated.<br>
Can be downloaded from: https://static.jasp-stats.org/PO-Files-0.12.1.zip<br>


#### *Translating the .po files*
In the next section the 'xx' stands for the unique two-letter code of the language.
See https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes for the complete list.

1. Translate the JASP .po interface file: JASP\_xx.po <br>Translate means here: Fill out the empty 'msgstr' field. Every  language has it's own specific .po file. This means that every language file contains a two-letter code that defines the (ISO) language name xx.po e.g.:
	* JASP_nl.po for Dutch translations.
	* JASP_de.po for German translations.

	The first version of this file is in fact a generated template file, without any translated message strings. All the message strings entries starting with 'msgstr' should be filled out with the proper translated string.

2. Translate the JASP R .po file: 	R-xx.po (located in jasp-desktop/Engine/JASP/po)
3. Translate the jaspGraphs .po file:	R-xx.po (located in jasp-desktop/Engine/jaspGraphs/po)

#### *Create the binary .mo and .qm files*
For translators that might have a complete build environment of JASP: edit Desktop.pro and almost on top set the  GENERATE\_LANGUAGE\_FILES = true. Then you need to specify the target language to translate. You can do this by defining it in the project build environment: Project\->Build Setrings\->Build environment and add the item: <br>
TARGET\_LANGUAGE\_CODE=nl  (e.g. For a Dutch translation. See the Language Code references below for complete list.)<br>
Rebuilding the Desktop will give you all the binary translation files.<br><br>
Translators who wish to test their translations on an already installed version of JASP:<br>Be sure that the first three items of the Section 4 are installed. The following commands from a terminal in the jasp-desktop folder can be run to create the .mo and .qm files.

1.	The Qt JASP interface .qm file: jasp\_xx.qm<br>From a terminal run: <br> > ~/Qt/5.14.2/clang_64/bin/lrelease ./Desktop/po/jasp\_xx.po -qm jasp\_xx.qm<br>
This creates a jasp\_xx.qm.<br>

2.	The JASP R related .mo file: R-JASP.mo<br>Start R from a terminal run: <br> > tools::update_pkg_po(paste0(getwd(),"/Engine/JASP"))<br>
This creates a R-JASP.mo in ./Engine/JASP/inst/po/xx/LC_MESSAGES

3. The jaspGraphs R related .mo file: R-jaspGraphs.mo<br>Start R from a terminal run: <br> > tools::update_pkg_po(paste0(getwd(),"/Engine/jaspGraphs"))<br>
This creates a R-jaspGraphs.mo in ./Engine/jaspGraphs/inst/po/nl/LC_MESSAGES

#### *Test binary translation files by copying them to an already installed JASP version*
Once the binaries are successfully created it is possible to test them in an already installed version of JASP. On startup JASP searches for all the typical language coded names, like JASP_nl.qm, and then tries to load that file with that specific language code. If one of those is found with a recognized language code it is added to the language preference menu, so that it can be chosen even at runtime.

Copy the files to the runtime environment:<br><br>
On  MacOS in a standard installation:<br>
Location of jasp\_xx.qm files: /Applications/JASP.app/Contents/Resources/Translations  
Location of .R-JASP.mo /Applications/JASP.app/Contents/MacOS/R/library/JASP/po/xx/LC\_MESSAGES<br>
Location of .R-jaspGraphs.mo /Applications/JASP.app/Contents/MacOS/R/library/jaspGraphs/po/xx/LC\_MESSAGES<br>


On  Windows in a standard installation:<br>
Location of jasp\_xx.qm: C:\Program Files\JASP\resources\Translations  
Location of R-JASP.mo C:\Program Files\JASP\R\library\JASP\R\xx\LC\_MESSAGES<br>
Location of R-jaspGraphs.mo C:\Program Files\JASP\R\library\jaspGraphs\R\xx\LC\_MESSAGES

#### *Putting the translations into production*
Translators familiar with  Github:<br> Can make a Pull Request containing their translated .po file. If this pull request is merged, the new language appears the next day in the nightly build. The new language is then activated by going to Preferences->Interface->Preferred Language and should appear in the dropdown menu.

Another way to have the translation in JASP is by opening an issue in the https://github.com/jasp-stats/jasp-issues/issues repository. The new Feature Request Issue should contain the translated .po files with some reference or zip with the translated files. Our developer will then help out.

#### *Questions*

If things are unclear or not working, please make an issue in the https://github.com/jasp-stats/jasp-issues/issues repository. Then all the developers are able to answer you. Or send a direct e-mail to f.a.m.meerhoff@uva.nl

### 6. References.

Internationalization with Qt : https://doc.qt.io/qt-5/qtquick-internationalization.html<br>
Writing source code for translation : https://doc.qt.io/qt-5/i18n-source-translation.html<br>
GNU gettext utilities : https://www.gnu.org/software/gettext/manual/gettext.html<br>
Language codes : https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
