
.clean <- function(value) {

	if (is.null(value))
		return ("")

	if (is.finite(value))
		return(value)

	if (is.na(value))
		return("NaN")

	if (value == Inf)
		return("\u221E")

	if (value == -Inf)
		return("-\u221E")

	NULL
}
