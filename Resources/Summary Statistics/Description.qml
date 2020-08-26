import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name			: "Summary Statistics"
	title			: qsTr("Summary Statistics")
	description		: qsTr("Summary Statistics module")
	icon			: "analysis-bayesian-ttest.svg"
	requiresData	: false
	version			: "0.13"
	author			: "JASP Team"
	maintainer		: "JASP Team <info@jasp-stats.org>"
	website			: "jasp-stats.org"
	license			: "GPL (>= 2)"

	GroupTitle
	{
		title:	qsTr("T-Tests")
		icon:	"analysis-bayesian-ttest.svg"
	}
	Analysis
	{
		menu:	qsTr("Bayesian Independent Samples T-Test")
		title:	qsTr("S.S. Bayesian Independent Samples T-Test")
		func:	"SummaryStatsTTestBayesianIndependentSamples"
	}
	Analysis
	{
		menu:	qsTr("Bayesian Paired Samples T-Test")
		title:	qsTr("S.S. Bayesian Paired Samples T-Test")
		func:	"SummaryStatsTTestBayesianPairedSamples"
	}
	Analysis
	{
		menu:	qsTr("Bayesian One Sample T-Test")
		title:	qsTr("S.S. Bayesian One Sample T-Test")
		func:	"SummaryStatsTTestBayesianOneSample"
	}

	GroupTitle
	{
		title:	qsTr("Regression")
		icon:	"analysis-bayesian-regression.svg"
	}
	Analysis
	{
		menu:	qsTr("Bayesian Correlation")
		title:	qsTr("S.S. Bayesian Correlation")
		func:	"SummaryStatsCorrelationBayesianPairs"
	}
	Analysis
	{
		menu:	qsTr("Bayesian Linear Regression")
		title:	qsTr("S.S. Bayesian Linear Regression")
		func:	"SummaryStatsRegressionLinearBayesian"
	}

	GroupTitle
	{
		title: qsTr("Frequencies")
		icon: "analysis-bayesian-crosstabs.svg"
	}
	Analysis
	{
		menu:	qsTr("Bayesian Binomial Test")
		title:	qsTr("S.S. Bayesian Binomial Test")
		func:	"SummaryStatsBinomialTestBayesian"
	}
	Analysis
	{
		menu:	qsTr("Bayesian A/B Test")
		title:	qsTr("S.S. Bayesian A/B Test")
		func:	"SummaryStatsABTestBayesian"
	}
}
