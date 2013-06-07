
results <- list()

for (field in options$main$fields)
{
  result <- try (silent = TRUE, expr = {
	r <- t.test(dataset[[field]])

	m <- as.numeric(r$estimate)
	t <- as.numeric(r$statistic)
	df <- as.numeric(r$parameter)
	p <- as.numeric(r$p.value)

	list(m, t, df, p)
  })

  if (class(result) == "try-error")
	result <- list("", "", "", "")

  results[[length(results) + 1]] <- result

}

results
