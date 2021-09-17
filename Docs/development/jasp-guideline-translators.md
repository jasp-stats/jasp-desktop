# Guideline for JASP translators

Thank you for your interest in the translation of JASP.
This document provides some first steps and general guidelines to maintain consistency of the translations.
Like the rest of JASP development, this is an ongoing process, and participation is welcome.

## Weblate
JASP uses the web-based localization tool [Weblate](https://hosted.weblate.org/projects/jasp/).
All you need to translate is a web browser — you can start as soon as you log in.

Once you save translations on *Weblate*, they are on their way into JASP.
(And if you are interested, you can read about [the pipeline past *Weblate*](#the-pipeline-past-weblate).)

### Log in to Weblate
Before you can add or change translations, you have to log in at <https://hosted.weblate.org/>.
To log in, you can either [register a new account](https://hosted.weblate.org/accounts/register/) or [sign in with a third-party account](https://hosted.weblate.org/accounts/login/), like your GitHub account.

Note that your translations will be publicly attributed to your account's full name and email address, so you may want to adjust these settings beforehand.

### JASP modules and Weblate components
Once you logged in, you can contribute translations to the JASP project at <https://hosted.weblate.org/projects/jasp/>.
JASP consists of many modules that offer different statistical analyses.
For example, the module `jaspAnova` offers analyses of variance (ANOVA).

On *Weblate*, every JASP module is split into two components.
The two components are distinguishable their name suffix (`-QML` or `-R`) that indicates their programming language:
- A component to define the user interface, written in the language [QML](https://en.wikipedia.org/wiki/QML).
- A component to define the statistical analysis, written in the language [R](https://en.wikipedia.org/wiki/R_(programming_language)).

For example, the `jaspAnova` module consists of the [jaspAnova-QML](https://hosted.weblate.org/projects/jasp/jaspanova-qml/) component for the interface and [`jaspAnova-R`](https://hosted.weblate.org/projects/jasp/jaspanova-r/) for the analysis code.
Clicking on a component shows an overview of the current translation status, as shown in this screenshot:

<img src="https://static.jasp-stats.org/images/Weblate-component.png" height="250px" />

### Adding languages
To add a new translation language to a component, click on the button *Start New Translation* and select from the list of available languages.
If you cannot find your language, please [report that to the JASP team](https://jasp-stats.org/feature-requests-bug-reports/).

### Contributing to existing languages
If you select an existing language, *Weblate* shows more details about the current translation status:

<img src="https://static.jasp-stats.org/images/Weblate-Chosen-Dutch.png" height="250px" />

When you select a set of strings within a component, *Weblate* opens the form to create/change a single translation string.
Note that all *Weblate* users have the same privileges to use this form: If you click the *Save* button after a change, it will remain until it is revised and saved again.
If you are unsure about a change, you can also use the *Suggest* button to inform other translators about your opinion.

<img src="https://static.jasp-stats.org/images/Weblate-Dutch.png" height="250px" />

There are a lot of options to get information about a string, and how to interact with it.
Fortunately, they are well explained in the *Documentation* that you can find in the right corner of the page:

<img src="https://static.jasp-stats.org/images/Weblate-Documentation.png" />

Once you press the *Save* button, your translated string is stored in *Weblate*.
You are done, and anyone else will be able to see your change.
Now you can log out of *Weblate*, or translate some more.

#### Glossary
*Weblate* offers glossaries, which are useful to maintain consistency of terminology translations, especially across modules.
Terms that are defined in the glossary are marked yellow in the translation form.
If a term is defined in the Weblate glossary in its proper context, you should use it.
If you disagree with a glossary term, don't just ignore it, but try to or discuss or change it first.

All translators can define new terminology into the glossary.
Each project can have an assigned glossary for any language as a shorthand for storing terminology.

#### Automatic suggestions
*Weblate* features machine translations in the *"Automatic suggestions"* tab.
This can be very useful as a starting point for a translation, but make sure to always scrutinize the output for errors.
Be aware that glossary terms are ignored and that these tools lack any understanding of the specific context of a string.

### The pipeline past Weblate
If you are interested, here is a summary what happens after you save translations on *Weblate*.
Knowledge of these steps are not necessary to contribute translations.

1. If translations on *Weblate* are added or saved, they are automatically sent to [GitHub](https://github.com/), so they can be integrated with the rest of the module's code.
    - This usually takes a few hours, but may also take days.
    - More technically, *Weblate* opens a [Pull Request](https://en.wikipedia.org/wiki/Distributed_version_control#Pull_requests) to the code repository of the translated module.
    - For example, translations to `jaspAnova-R` or `jaspAnova-QML` components are sent to the [jaspAnova repository on GitHub](https://github.com/jasp-stats/jaspAnova).
2. On *GitHub*, one of the module developers will manually check and merge the translation into the module's repository.
3. Every week, *GitHub* automatically updates the translation files.
   Now they are ready to be compiled into an executable program.
    - If new translatable strings were added to the code, they will become available on *Weblate* for translation.
    - More technically, this step involves multiple sub-tasks and file conversions (e.g. from `.po` files to `.qm` and `.mo`); if you are interested, you can read [more technical details here](translate.md).
4. Once a new JASP executable is compiled, all new translations become part of the program.
    - Every night, JASP is automatically compiled as a fresh [nightly build](https://static.jasp-stats.org/Nightlies/) that you can download and test.
    - Alternatively, you can also compile JASP on your own computer by following the [JASP building guide](jasp-building-guide.md).
    - Finally, your translations will become available to everybody, once the JASP team releases a [new version of JASP](https://jasp-stats.org/release-notes/).

## Translation conventions
This section lists general rules for translation.

## Statistical terms
Use statistical terms as they occur in introductory statistics textbooks and courses in your language.
Users should be able to recognize established terms in JASP's interface.
If you deem it useful to offer alternative terms (e.g. that may be suitable for beginners), provide them in the help file.

### Capitalization
For names of titles, groups and sections in the interface, all words should be capitalized.
For names within such groupings, e.g. the labels of the controls, only the first word should be capitalized.
For instance, the *Visual Modeling* module is called *'Visual Modeling'* and one of the analyses is called *'Generalized Linear Modeling'*.
But if we look at the section *'Results Display'*, the checkbox label *'Model plot'* only starts with a capital in the first word.

### Address the user formally
The JASP interface occasionally addresses the user directly, e.g. 'what would *you* like to do?'.
Some languages have multiple ways to translate the English 'you', ranging from formal to informal ways (see [T-V distinction](https://en.wikipedia.org/wiki/T%E2%80%93V_distinction)).
If you translate JASP into such a language, you should choose a formal way of addressing the user.
For example, in Dutch use the pronoun *'U'* instead of *'jij'*, and in French use *'vous'* instead of *'tu'*.
