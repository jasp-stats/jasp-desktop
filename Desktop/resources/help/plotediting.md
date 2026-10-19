Plot Editing
============

Although we try our best, sometimes plots created by JASP need some tweaks. The current editor allows you to modify the axes of a plot, but future versions of JASP should provide more options.

### Basic options

In the top left you can select which axis you want to modify. You can select either the x-axis or the y-axis. All of the options below modify either the x-axis or the y-axis, depending on which is currently selected.

#### Title

*   `Show title`: Should the title of the selected axis be shown?
*   `Title`: Specify what to put on the axis title. Usually, this is plain text but sometimes it is an R expression, for example when a title contains greek or mathematical symbols. See [Parse title as R expression](#parse_as_r_expression)) for more details on that.

#### Ticks

Ticks consist of two components: positions and labels. Very often, but not always, the labels simply show the positions, for example, a position at -4 is indicated with the label "-4".

If you want the labels and positions to be the same, it is convenient to select `Specify sequence`. If you select `Specify sequence` then the ticks are fully determined by three numbers. The left and right endpoints are determined by `from` and `to` respectively. After every `steps` numbers, there will be a label. For example if `from` is -4, `to` is 4, and `steps` is 2, then you would get ticks at -4, -2, 0, 2, 4.

Sometimes you may want the labels to differ from the positions, or maybe you do not want equally spaced labels. In that case, you can select `Set manually`. Once selected, a table is shown that allows for individually modifying the positions and labels. If you left-click on a column, two green plus icons appear and one red cross. The left and right plus icons allow you to insert a new position and label on the left and right of the selected column. The red cross allows you to delete the selected column.

Note that for discrete axes, the ticks cannot be adjusted.

### Advanced options

#### Parse title as R expression

Some figures have complicated titles, which cannot be typeset in plain text. For example, to put the symbol δ (delta) on an axis you can type "delta" and enable this option. However, you can also create more complicated equations, such as `over(alpha, beta)`. For more details, see [here](https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html) for more details.

#### Limits

The limits consist of two values: `Upper limit` and `Lower limit`. These values determine the outer end points of an axis or plotable area. For example, even though the axis positions are going from 80 to 100 in steps of 10, it might be that there is a data point at 102. The limits provide some leeway to show all data without being forced to increase `to` to 105. You can select one of the following options

*   `Based on data`: Use an automatic default for the limits that is determined based on the data instead of following the value of the ticks.
*   `Based on ticks`: Automatically rescale the limits with the ticks. For example, if you increase ticks' `to` to some value larger than the `Upper limit`, this will automatically set `Upper limit` to `to`.
*   `Set manually`: Specify two values for the `Upper limit` and `Lower limit` manually.

When using `Set manually`, take these two warnings to heart:

1.  Any data that falls outside of the limits is not shown.
2.  To adjust the `Ticks` you now also must adjust the `Limits`. For example, if the `Upper limit` is set to 4, and you now set `to` to 8, then the right limit of the figure remains at 4, so no ticks larger than 4 will be shown.

### References

Almost all plots in JASP use the R package ggplot2. This package is an implementation of the grammer of graphics. For more information, see

*   Wickham, H. (2016). _[ggplot2: Elegant Graphics for Data Analysis](https://ggplot2-book.org/)_ (3rd ed.). Springer-Verlag.
*   [https://ggplot2.tidyverse.org/index.html](https://ggplot2.tidyverse.org/index.html)
*   Wilkinson, L. (2005). _[The grammar of graphics](https://dx.doi.org/10.1007/0-387-28695-0)_ (2nd ed.). Springer.
*   [https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html](https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html)
