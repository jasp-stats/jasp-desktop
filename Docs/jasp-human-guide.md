The JASP Guide to Writing Software for Humans
=============================================

Software is not just for statisticians. Humans use software too. This guide is intended highlight some of the differences between writing for statisticians (i.e. what is generally considered 'normal' or 'good enough' in R packages), and writing software for humans.

These boil down to the three areas

- Good error messages
- Thorough testing 
- Automatically check assumptions

### Good error messages

In many R packages, quite cryptic error message can be thrown. For example, the *lavaan* package when provided with an empty model presents the following error message:

`Error in start.idx[i]:end.idx[i] : NA/NaN argument`

Of course, the user should not provide an empty model, but most people acquainted with murphy's law know that this probably still happens all the time. Good error messages prevent:

 - people being confused
 - them complaining / asking on forums / emailing you
 - you having to respond to said complaint / email

In summary, good error messages make JASP awesome and save you time.
 
Hence, in developing an analysis, it is good to think through each and every sort of mistake the user could make, and test for each of these mistakes before running the analysis (More about this in testing below).

Should a mistake be found, the error message should use calm and neutral language, should provide a short description of the problem (this does not need to be complete, but describing it in a way that the user can type it into google can be helpful). If the problem has a simple and clear solution, it can be appropriate to incorporate this into the message as well.

The sorts of errors that should be handled, can be found through thorough testing.

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
