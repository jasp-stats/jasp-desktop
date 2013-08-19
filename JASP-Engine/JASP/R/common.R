
.clean <- function(value) {

	if (is.finite(value))
		return(value)

	if (is.na(value))
		return("NaN")

	if (value == Inf)
		return("Inf")

	if (value == -Inf)
		return("-Inf")

	NULL
}
