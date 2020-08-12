context("[Audit] Classical Benford's Law")

options <- jaspTools::analysisOptions("auditClassicalBenfordsLaw")
options$.meta <- list(values = list(containsColumn = TRUE))
options$benfordsLawPlot <- TRUE
options$values <- "value"
set.seed(1)
results <- jaspTools::run("auditClassicalBenfordsLaw", "sinoForest.csv", options)

test_that("Observed Percentages vs. Benford's Law plot matches", {
	plotName <- results[["results"]][["benfordsLawContainer"]][["collection"]][["benfordsLawContainer_benfordsLawPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "observed-percentages-vs-benford-s-law", dir="auditClassicalBenfordsLaw")
})

test_that("<b>Table 2.</b> Descriptive Statistics results match", {
	table <- results[["results"]][["benfordsLawContainer"]][["collection"]][["benfordsLawContainer_benfordsLawTable"]][["data"]]
	expect_equal_tables(table,
		list(231, 1, "30.1%", "29.92%", 124, 2, "17.61%", "16.06%", 97, 3,
			 "12.49%", "12.56%", 70, 4, "9.69%", "9.07%", 64, 5, "7.92%",
			 "8.29%", 54, 6, "6.69%", "6.99%", 40, 7, "5.8%", "5.18%", 54,
			 8, "5.12%", "6.99%", 38, 9, "4.58%", "4.92%"))
})

test_that("<b>Table 1.</b> Goodness-of-fit Test results match", {
	table <- results[["results"]][["benfordsLawContainer"]][["collection"]][["benfordsLawContainer_benfordsLawTestTable"]][["data"]]
	expect_equal_tables(table,
		list(772, 8, "X<unicode><unicode>", 0.468206381300367, "Chi-square",
			 7.652))
})