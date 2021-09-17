# Guideline for JASP translators

Thank you for your interest in translating JASP.
This document provides some first steps and general guidelines to maintain consistency.
As part of JASP's development, translating is an ongoing process, and participation is welcome.

## Weblate
To make translating easy, JASP uses the web-based localization tool [Weblate](https://hosted.weblate.org/projects/jasp/).
All you need is a web browser, and you can start as soon as you log in.

Once you save translations on *Weblate*, they will be incorporated in JASP.
(Below you can read [how your translations get into JASP after you commit them to *Weblate*](#how-your-translations-get-into-jasp-after-you-commit-them-to-weblate).)

### Log in to Weblate
Before you can add or modify translations, you have to log in at <https://hosted.weblate.org/>.
To log in, you can either [register a new account](https://hosted.weblate.org/accounts/register/) or [sign in with a third-party account](https://hosted.weblate.org/accounts/login/) such as your GitHub account.

Note that your translations will be publicly attributed to your account's full name and email address, so you might want to adjust these settings beforehand.

### JASP modules and Weblate components
Once you logged in, you can contribute translations to the JASP project at <https://hosted.weblate.org/projects/jasp/>.
JASP consists of various modules each offering several statistical analyses.
For example, the module `jaspAnova` offers both frequentist and Bayesian analysis of variance (ANOVA), repeated measures analysis of variance (RM ANOVA), analysis of covariance (ANCOVA), and multivariate analysis of variance (MANOVA). 

On *Weblate*, every JASP module is split into two components.
The two components have the suffixes (`-QML` or `-R`) that indicate their programming language:
- The [QML](https://en.wikipedia.org/wiki/QML) component refers to strings (i.e., text) found in the user interface, that is, the left-hand panel in JASP
- The [R](https://en.wikipedia.org/wiki/R_(programming_language)) component refers to strings found in the output pane that appear in the right-hand panel in JASP resulting from statistical analyses computed using R. 

For example, the `jaspAnova` module consists of the components [jaspAnova-QML](https://hosted.weblate.org/projects/jasp/jaspanova-qml/) for the interface and [jaspAnova-R](https://hosted.weblate.org/projects/jasp/jaspanova-r/) for the analysis code.
Clicking on a component shows an overview of the current translation status:

<img src="https://static.jasp-stats.org/images/Weblate-component.png" height="250px" />

### Adding languages
To add a new language to translate to a component, click on the button *Start New Translation* (see screenshot above) and select from the list of available languages.
If you cannot find your language, please [report that to the JASP team](https://jasp-stats.org/feature-requests-bug-reports/).

### Contributing to existing languages
If you select an existing language, *Weblate* shows more details about the current translation status:

<img src="https://static.jasp-stats.org/images/Weblate-Chosen-Dutch.png" height="250px" />

When you select a set of strings within a component, *Weblate* opens the form to create or change a single translation string.
Strings are the smallest unit to translate, and they can vary in length from just one character to entire paragraphs, as decided by the module developers.

Note that all *Weblate* users have the same privileges to use this form: If you click the *Save* button after a change, the change will remain until it is revised and saved again.
If you are unsure about a change, you can also click the *Suggest* button to inform other translators about your opinion.

<img src="https://static.jasp-stats.org/images/Weblate-Dutch.png" height="250px" />

There are a lot of options to get additional information about a string, and how to interact with it.
Fortunately, they are well explained in the *Documentation* that you can find in the right corner of the page:

<img src="https://static.jasp-stats.org/images/Weblate-Documentation.png" />

Once you press the *Save* button, your translated string is stored in *Weblate*.
Once saved, everyone will be able to see your change.
Now you can log out of *Weblate*, or translate some more.

#### Glossary
To help translators, *Weblate* offers glossaries to maintain consistency of translated terminology, especially across modules.
Terminology defined in the glossary are marked yellow in the translation form. 
If a term is defined in the Weblate glossary in its proper context, you should use it.
If you disagree with a translate term, don't just ignore it. 
Instead, try to discuss it with other translators, or change it in the glossary first.

Note that all translators can define new terminology into the glossary.
Each project can have an assigned glossary for any language as a shorthand to store terminology.

#### Automatic suggestions
*Weblate* can suggest machine translations in the *Automatic suggestions* tab.
This can be very useful as a starting point for a translation, but make sure to always scrutinize the output for errors.
Note that glossary terms are ignored and that these tools lack any understanding of the specific context of a string.

## Translation conventions
This section lists general rules for translation.

## Statistical terms
Use statistical terms as they occur in introductory statistics textbooks and courses in the target language.
Users should be able to recognize established terminology in JASP's interface.
If you deem it useful to offer alternative terms (e.g., that may be more suitable for beginners), provide them in the help file.

### Capitalization
For names of titles, groups and sections (tabs such as *Advanced*) in the interface, all words should be capitalized.
For names within such groupings, e.g., the labels of the controls, only the first word should be capitalized.
For instance, the *Visual Modeling* module is called *'Visual Modeling'* and one of the analyses is called *'Generalized Linear Modeling'*.
All words in the section title are also capitalized *'Results Display'*, but only the first words of elements within the section are capitalized, e.g., the checkbox label *'Model plot'*. 

### Address the user formally
The JASP interface occasionally addresses the user directly, e.g., 'what would *you* like to do?'.
Some languages have multiple ways to translate the English 'you', which could be formal or informal (see [T-V distinction](https://en.wikipedia.org/wiki/T%E2%80%93V_distinction)).
If you translate JASP into such language, you should choose the formal form to address the user.
For example, in Dutch please use the pronoun *'U'* instead of *'jij'*, in French please use *'vous'* instead of *'tu'*, and in German please use *'Sie'* instead of *'du'*.

## How your translations get into JASP after you commit them to Weblate
This section provides a summary of what happens to your translation once it is saved on *Weblate*.
Knowledge of these steps are not necessary to contribute translations.

1. If translations on *Weblate* are added or saved, they are automatically sent to [GitHub](https://github.com/), so they can be integrated with the rest of the module's code.
    - This usually takes a few hours, but may also take days.
    - More technically, *Weblate* opens a [Pull Request](https://en.wikipedia.org/wiki/Distributed_version_control#Pull_requests) to the code repository of the translated module.
    - For example, translations of the `jaspAnova-R` or `jaspAnova-QML` components are sent to the [jaspAnova repository on GitHub](https://github.com/jasp-stats/jaspAnova).
2. On *GitHub*, one of the module developers will manually check and merge the translation into the module's repository.
3. Every week, *GitHub* automatically updates the translation files.
   Now they are ready to be compiled into an executable program.
    - If new translatable strings were added to the code, they will become available on *Weblate* for translation.
    - More technically, this step involves multiple sub-tasks and file conversions (e.g., from `.po` files to `.qm` and `.mo` files); if you are interested, you can read [more technical details here](translate.md).
4. Once a new JASP executable is compiled, all new translations become part of the program.
    - Every night, JASP is automatically compiled as a fresh [nightly build](https://static.jasp-stats.org/Nightlies/) which you can download and test.
    - Alternatively, you can also compile JASP on your own computer by following the [JASP building guide](jasp-building-guide.md).
    - Finally, your translations will become available to everybody, once the JASP team releases a [new version of JASP](https://jasp-stats.org/release-notes/).


