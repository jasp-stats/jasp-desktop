import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "Frequencies"
	title		: qsTr("Frequencies")
	description	: qsTr("This module offers the standard Frequentist and Bayesian frequency analyses.")
	icon		: "analysis-classical-crosstabs.svg"
	version		: "0.13"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"

	GroupTitle
	{
		title:	qsTr("Classical")
		icon:	"analysis-classical-crosstabs.svg"
	}

	Analysis
	{
		title:	qsTr("Binomial Test")
		func:	"BinomialTest"
	}

	Analysis
	{
		title:	qsTr("Multinomial Test")
		func:	"MultinomialTest"
	}

	Analysis
	{
		title:	qsTr("Contingency Tables")
		func:	"ContingencyTables"
	}

	Analysis
	{
		title:	qsTr("Log-Linear Regression")
		func:	"RegressionLogLinear"
	}

	Separator {}

	GroupTitle
	{
		title:	qsTr("Bayesian")
		icon:	"analysis-bayesian-crosstabs.svg"
	}

	Analysis
	{
		menu:	qsTr("Binomial Test")
		title:	qsTr("Bayesian Binomial Test")
		func:	"BinomialTestBayesian"
	}

	Analysis
	{
		menu:	qsTr("A/B Test")
		title:	qsTr("Bayesian A/B Test")
		func:	"ABTestBayesian"
	}

	Analysis
	{
		menu:	qsTr("Multinomial Test")
		title:	qsTr("Bayesian Multinomial Test")
		func:	"MultinomialTestBayesian"
	}

	Analysis
	{
		menu:	qsTr("Contingency Tables")
		title:	qsTr("Bayesian Contingency Tables")
		func:	"ContingencyTablesBayesian"
	}

	Analysis
	{
		menu:	qsTr("Log-Linear Regression")
		title:	qsTr("Bayesian Log-Linear Regression")
		func:	"RegressionLogLinearBayesian"
	}
}
