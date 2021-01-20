# Guideline for JASP translators

## Introduction
In this document we keep track of some general guidelines that translators of JASP should follow during the tanslation process. This is an ongoing process and this document should help to maintain consistancy. 

## Weblate
JASP uses the online localisation tool 'Weblate' for its translations. <br>To contribute to the translations of JASP you need to make an account for Weblate:<br>
* Open the page https://weblate.org/en-gb/  
* Press the persons icon  
* And register your new account in https://hosted.weblate.org/accounts/register/
<br>
Once you've registered your account you can open the JASP project at https://hosted.weblate.org/projects/jasp/
Here you can find an overview of all the different modules JASP consists of and the translations it contains. Every single module contains two components: an interface component and an R component. 
E.g. the Anova module consists of the jaspANOVA-QML component for the interface and the jaspANOVA-R component for the R part. Pressing on the component show the current translation of the component.<br><br>
<img src="https://static.jasp-stats.org/images/Weblate-component.png" width="800" height="250" /> <br>For adding a new translation:  
* Press the Start New Translation button.  
* Choose your language from the available Languages list.  
* If your language is not in the list, report this to Weblate with the 'Cannot find your language in the list above?' link. <br>
When you select your language from the list you will get an overview of the translated and not yet translated strings.<br><br>
<img src="https://static.jasp-stats.org/images/Weblate-Chosen-Dutch.png" width="800" height="250" /> <br>
Selecting the part of strings you want to change will bring you to the form where you do the actual translation work. Be aware that all Weblate users have the same rights filling in this form.<br><br>
<img src="https://static.jasp-stats.org/images/Weblate-Dutch.png" width="800" height="250" /> <br><br>
There are a lot of options to be chosen here but all are well described in the online documentaion you can find in the right corner of the page. <br><img src="https://static.jasp-stats.org/images/Weblate-Documentation.png" width="80" height="100" /> <br>The working procedure is now as follows:  
*  When you save (Save button) your work, your translated strings are stored in Weblates repository. And everybody will see your work.  
*  Once you have committed your work (the Manage/Commit button in the previous screen) a PR (pull request) is made to the JASP github repository belonging to this component.  For instance see the [JASP Anova repository](https://github.com/jasp-stats/jaspAnova) for the JASP Anova module repository.  
*  This PR is then merged into this JASP module repository by the responsable developer of this repository.  
*  During the nightly build new JASP binary translation files (.qm and .mo) are generated. These files are added to the setup of JASP, so the new translations should be available in this [nightly](http://static.jasp-stats.org/Nightlies/) made version.  
*  Every week new JASP .po files are generated containing the new translatable strings inside JASP. They will then be merged into Weblate. Some component may then have new strings to translate. 

## Translation of statistical terms
1. Weblate has the functionality of defining glossaries. Each project can have an assigned glossary for any language as a shorthand for storing terminology. Consistency is more easily maintained this way. If a term is defined in the Weblate glossery in its proper context this definition should be used. Definitions defined in the glossery can be exported and imported.
2. When in doubt, please use the statistical terms as they occur in introductory statistics textbooks and courses in your language. We would like users to recognize the terms that are used. If you deem this useful you can mention alternative terms in the help file.   
* 3. Weblate has the possibility of giving a suggestion of the translation in the "Automatic suggestions" tab. Three automatic translate machines are available i.e. Google Translate, Microsoft Translator and DeepL. Above that, Weblate offers you the possibility to generate a complete translation of a component.

## Capitalization
As a general rule we use the following conventions. For names of titles, groups and sections all the words are capitalized. For all the subnames in those groups, e.g. the labels of the controls, those names only start with a capital. For instanca the Visual Modeling module is named 'Visual Modeling' and one of the analysis is called 'Generalized Linear Modeling'. But if we look at the section 'Results Display' the checkbox 'Model plot' only starts wit a capital.


## Formal or Familiar 
In JASP we have choosen for a formal way of addressing a user. In some languages there are more levels of speech but in general we use the formal level of speech. So in Dutch we use the persons forms 'U' instead of 'jij' and in French 'vous' instead of 'tu'. 



