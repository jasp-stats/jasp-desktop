Rules for the translation of JASP source files
==============================================

**1. Qml Files**

    a. All literal strings e.g. in a label or text field should be embedded in the
       qsTr function: e.g. _qsTr_ ("Log to file ")
    b. Use %x to Insert parameters into a string.
       Different languages put words together in different orders so it is not a
       good idea to create sentences by concatenating words and data.
       Instead, use % to insert parameters into strings. For example, the
       following snippet has a string with two number parameters %1 and %2.
       These parameters are inserted with the .arg() function.
       Text{ text: qsTr(“File %1 of %2).arg(counter).arg(total) }
       %1 refers to the first parameter and %2 refers to the second parameter
       so this code produces output like: "File 2 of 3".

**2. C++ files**

    a. In the .cpp files you should the tr() function for all titeral text
       e.g. errorMsg = tr("Refreshing the analysis may change the results.");
    b. Similar as mentioned above use the % character for parameters in a
       string
       for example : errorMsg = tr("Cannot find a source %1 for VariableList
       %2").arg(dropKey).arg(listView->name())

**3. R-files**

    a. All literal strings e.g. in titles or messages must be embedded in the
       gettext() function.
       e.g. title=gettext("Hypothesis")
    b. Unicode character may not appear in the literals strings as output
       strings:
       e.g. title="McDonald's \u03C9" must become gettextf(“McDonald's
       %s”,” \u03C9") instead of title=gettext("McDonald's \u03C9")
	   The same is true for a single % character in a gettext. It must be transformed 
	   to a gettextf with a double %%. Please report if you know a better solution.
    c. All paste and paste0 functions must be replaced by the gettext or the
       gettextf functions. For example:
       overtitle = paste0(100 * options$confidenceIntervalInterval, "% CI for
       Proportion")) becomes :
       overtitle = gettextf("%i%% CI for Proportion", 100 *
       options$confidenceIntervalInterval))
       But it is sometimes also possible to use a combination:
       Paste0(gettext(“This is ok”), gettext(“Not very useful but possible”))
    d. Covert sprintf() into gettextf() directly.
    e. But further, immediately after % may come 1$ to 99$ to refer to a
       numbered argument: this allows arguments to be referenced out of
       order and is mainly intended for translators of error messages. If this is
       done it is best if all formats are numbered: if not the unnumbered ones
       process the arguments in order. See the example. This notation allows
       arguments to be used more than once, in which case they must be
       used as the same type (integer, double or character).
       E.g.:      
       message <- sprintf("Some entries of %s were not understood. These
       are now grouped under '%s'.", options[["colorNodesBy"]], undefGroup)
       Becomes:
       message <- gettextf("Some entries of %1$s were not understood.
       These are now grouped under '%2$s'.", options[["colorNodesBy"]],undefGroup)
    f. Be careful with expressions. Must be considered one by one:
       E.g.
       xName<- expression(paste("Population proportion", ~theta))
       Becomes:
       xName <- bquote(paste(.(gettext("Population proportion")), ~theta))
