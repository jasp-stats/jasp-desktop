The JASP Guide to Writing Software for Humans
=============================================

Software is not just for statisticians. Humans use software too. This guide is intended highlight some of the differences between writing for statisticians (i.e. what is generally considered 'good enough' in R packages), and writing software for humans.

These boil down to the three areas

- Good error messages
- Thorough testing 
- Automatically check assumptions

### Good error messages

In many R packages, quite cryptic error message can be thrown. For example, passing an Infinity into a t-test gives the following error message:

    > t.test(c(2,7,3,Inf))
    Error in if (stderr < 10 * .Machine$double.eps * abs(mx)) stop("data are essentially constant") : 
  missing value where TRUE/FALSE needed

Of course, the user should not have an infinity in their model, but most people acquainted with murphy's law know that this probably still happens all the time. Good error messages prevent:

 - people being confused
 - them complaining / asking on forums / emailing you
 - you having to respond to said complaint / email

In summary, good error messages make JASP awesome and save you time in the long run.
 
Hence, in developing an analysis, it is good to think through each and every sort of mistake the user could make, and test for each of these mistakes at the start of the analysis (More about this in testing below).

Error messages can either be placed over the top of the results table, or put in the footnotes. In general, an error which makes the whole analysis meaningless, should be placed over the top of the results table. A t-test where the user has specified an independent variable with three levels would be one such example. In contrast, if the error only affects one or a handful of values in the table, then an NaN should be placed in that cell, and a footnote marker added.

### Writing for internationalization

Internationalization(i18n) is the adapting of module to different languages. An inspiring news is that JASP is moving towards i18n, which means that the analysis and modules you developed will be translated into different languages. Therefore, it is necessary to pay attention to the readability and flexibility of the message during writing.
Passing your message with [gettext()](https://en.wikipedia.org/wiki/Gettext), JASP will generate the string  to be translated automatically in a `.po` file.

**Placeholders**

If you have multiple placeholders, you can provide them inside `gettext()` as `$1`, `$2`, and `$3` and so on. 

Some examples:

_Not good:_

  ```r
  gettextf("Number of factor levels is %s in %s", "{{factorLevels.amount}}", "{{variables}}")
  ```
 

_Good:_
	
  ```r
  gettextf("Number of factor levels is %1$s in %2$s", "{{factorLevels.amount}}", "{{variables}}")
  ```

**Use Unicode everywhere**

Internationalization includes support for multiple character sets (usually we recommend Unicode), e.g, use `\u03B2` is better than a `β` symbol in messages. This allows your writing characters available on multiple language environments.

### Thorough testing

It is important that software is thoroughly tested, and that a proactive approach (proactively thinking through what sorts of problems may occur) is taken rather than reactive (simply waiting until people complain about things not working).

If a program has conditional execution, (i.e. `if` statements), then tests should be run to test every possible path through the program.

A good approach is to craft a set of data files, all of them defective or odd in some particular way. For example, for a t-test, good data sets might be:

 - completely empty
 - only one level in the independent variable
 - more than two levels in the independent variable
 - lots of missing data
 - really, really, large values
 - really, really, small values
 - only one value in each group
 
It's also worth seeing the way that SPSS handles these too, because, for all it's faults, it probably does this a heck of a lot better than R.
 
### Automatically check assumptions

The beauty of computers is that they never forget. People do forget to do things, and checking assumptions is one of them. So analyses should always check assumptions, even when the user forgets to.

If the user does not check for an assumption, and the assumption is violated, then good practice is to add a footnote to the affected value explaining the violation. If the user explicitly checks for the violation (i.e. by selecting the "test for inhomogeneity of variances") then these footnotes need not be displayed; In both cases, assumptions are checked.

Additionally, it is nice to warn users if the data they have provided are too few. For example, I think the Χ² statistic for contingency tables is suspect if a cell contains less than 5 counts.

