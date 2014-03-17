
JASP style guide for R
======================

All R code for JASP should follow the [google style guide](https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml).

However a couple of exceptions should be observed

1. function names that aren't the name of an analysis (i.e. those
privately used by the JASP R package) begin with a . and lowercase.

    `NotLikeThis()`

    `.butLikeThis()`

2. tabs are used rather than two spaces (you should set up your editor so that tabs appear four spaces wide)
