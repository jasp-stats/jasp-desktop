# this file exists because ggplot has a habit of changing standards over time
# it also imports some functions from ggplot

#' @importFrom ggplot2 waiver aes xlab ylab element_blank



getMajorSource <- function(gb) {

    # gb: output from ggplot2::ggbuild(ggplot)
     return(
        switch (graphOptions("ggVersion"),
            "2.2.1.9000" = list(
                x = gb$layout$panel_params[[1]]$x.major_source,
                y = gb$layout$panel_params[[1]]$y.major_source
            ),
            list(
                x = gb$layout$panel_ranges[[1]]$x.major_source,
                y = gb$layout$panel_ranges[[1]]$y.major_source
            )
        )
    )

}

getRanges <- function(gb) {

    return(
        switch (graphOptions("ggVersion"),
            "2.2.1.9000" = list(
                x = gb$layout$panel_params[[1]]$x.range,
                y = gb$layout$panel_params[[1]]$y.range
            ),
            list(
                x = gb$layout$panel_ranges[[1]]$x.range,
                y = gb$layout$panel_ranges[[1]]$y.range
            )
        )
    )


}


is.waive <- function (x) inherits(x, "waiver")
# waiver <- ggplot2::waiver
