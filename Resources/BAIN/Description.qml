import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	title: 			qsTr("Bain")
	name:			"BAIN"
	description: 	qsTr("A module for computing approximated adjusted fractional Bayes factors for equality, inequality, and about equality constrained hypotheses.")
	version:		"0.1.0"
	author:			"Herbert Hoijtink, Joris Mulder & Xin Gu"
	maintainer:		"Koen Derks <k.derks@nyenrode.nl>"
	website:		"www.informative-hypotheses.sites.uu.nl/software/bain/"
	license:		"GPL (>= 3)"
	icon:			"bain-module.svg"

	GroupTitle
	{
		title:		qsTr("T-Tests")
		icon: 		"analysis-bain-ttest.svg"
	}

	Analysis
	{
		menu:		qsTr("Welch's T-Test")
		title:		qsTr("Bain Welch's T-Test")
		func:		"BainTTestBayesianIndependentSamples"
	}

	Analysis
	{
		menu: 		qsTr("Paired Samples T-Test")
		title:		qsTr("Bain Paired Samples T-Test")
		func:		"BainTTestBayesianPairedSamples"
	}

	Analysis
	{
		menu:  		qsTr("One Sample T-Test")
		title:		qsTr("Bain One Sample T-Test")
		func:		"BainTTestBayesianOneSample"
	}

	GroupTitle
	{
		title: 		qsTr("ANOVA")
		icon: 		"analysis-bain-anova.svg"
	}

	Analysis
	{
		menu:   	qsTr("ANOVA")
		title:		qsTr("Bain ANOVA")
		func:		"BainAnovaBayesian"
	}

	Analysis
	{
		menu:   	qsTr("ANCOVA")
		title:		qsTr("Bain ANCOVA")
		func:		"BainAncovaBayesian"
	}

	GroupTitle
	{
		title: 		qsTr("Regression")
		icon: 		"analysis-bain-regression.svg"
	}

	Analysis
	{
		menu:   	qsTr("Linear Regression")
		title:		qsTr("Regression")
		func:		"BainRegressionLinearBayesian"
	}
}
