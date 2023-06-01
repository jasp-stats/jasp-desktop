# R Style Guide for JASP programming

In the past we've had many different styles for the `R` analyses, as people from different backgrounds have been creating analyses for JASP. This has created different structures and different styles which makes it fundamentally hard to read our code. With the new backend of `jaspResults`, we have an opportunity to fix this issue. This document outlines a style guide for our R files. A `linter` will be implemented on Travis CI to preclude style deviations from happening in the future: any code that does not follow this style guide will be rejected because of errors in the CI checks. In other words: _strictly adhere to the style guide._

This style was influenced by the [Google style guide](https://google.github.io/styleguide/Rguide.xml) and how our repository behaves on GitHub.

## Style Guide

- `camelCasing` or `lowercase` for variable names: Read our [option names guide](https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/guide-option-names.md#use-camelcase) on how for instance abbreviations should be treated. 
- No tabs allowed, use 2 spaces. Otherwise the file will be malformed on GitHub.
- Code may not be wider than 120 characters. This makes it possible to view 2 files side-by-side on a reasonable screen and it makes sure there is no sideways scrolling on GitHub. It does still allow wider than regular code (regular limit is 80 characters). Adjust your editor to show you the margin!
- Use `<-` not `=` for assignment.
- See [spacing](https://google.github.io/styleguide/Rguide.xml#spacing). Additionally, wherever it improves legibility the code may be aligned by using extra spaces (_But stay within 120 characters_)
```r
  hello <- someFunc(x = "something", y = "ok",     z = 1)
  ok    <- someFunc(x = "socool",    y = "nope",   z = 2)
  nice  <- someFunc(x = "nice",      y = "yippie", z = 3)
```
Functions with many arguments may be made more legible by making them vertical and aligning the equals signs:
```r
  coolFunction(
    arg1      = "hello",
    arg2      = "world",
    numbers   = 1:10,
    ok        = letters[1:10],
    morestuff = TRUE,
    lessstuff = FALSE
  )
```
- An opening curly brace should never go on its own line; a closing curly brace should always go on its own line. See [curly braces](https://google.github.io/styleguide/Rguide.xml#curlybraces). Omit curly braces where possible, such as with single-statement if-blocks:
```r
  if (condition)
    doSomething(with = argument)
```
- See [else](https://google.github.io/styleguide/Rguide.xml#else). If it fits, single-line if-else statements are also allowed:
```r
if (condition) doSomething() else doSomethingElse()
```
- Make function returns explicit in if statements.
```r
  myFunc <- function(sth) {
    if (sth > 10) {
      return("bigger")
    } else if (sth > 5) {
      return("big")
    } else {
      return("small")
    }
  }
```
otherwise, return may be used but is not required.
```r
  myFunc <- function(sth) {
    sth2 <- sth^2
    sth2
  }
```
- Main analysis functions have the format `SomeCoolAnalysis()`, as well as all other functions called by the user interface.
- R internal functions have the format `.[name]ReadData()` where `[name]` is a unique short identifier of the analysis.
- Follow the (`analysis-skeleton.R`)[https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/analysis-skeleton.R] file for structuring analyses.
- Each file starts with the following copyright notice (edit the date if necessary):
```r
#
# Copyright (C) 2013-2022 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
```
- Put comments where necessary.
- Denote to-dos with `# TODO(username) my thing todo in this code section`.
