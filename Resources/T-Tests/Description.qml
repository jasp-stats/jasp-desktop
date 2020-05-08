import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "T-Tests"
	title		: qsTr("T-Tests")
	description	: qsTr("This module offers the standard Frequentist and Bayesian T-Tests analyses.")
	version		: "0.13"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"
	icon		: "analysis-classical-ttest.svg"

	GroupTitle
	{
		title:	qsTr("Classical")
		icon:	"analysis-classical-ttest.svg"
	}
	Analysis
	{
		title:	qsTr("Independent Samples T-Test")
		func:	"TTestIndependentSamples"
	}
	Analysis
	{
		title:	qsTr("Paired Samples T-Test")
		func:	"TTestPairedSamples"
	}
	Analysis
	{
		title:	qsTr("One Sample T-Test")
		func:	"TTestOneSample"
	}

	Separator{}

	GroupTitle
	{
		title:	qsTr("Bayesian")
		icon:	"analysis-bayesian-ttest.svg"
	}
	Analysis
	{
		menu:	qsTr("Independent Samples T-Test")
		title:	qsTr("Bayesian Independent Samples T-Test")
		func:	"TTestBayesianIndependentSamples"
	}
	Analysis
	{
		menu:	qsTr("Paired Samples T-Test")
		title:	qsTr("Bayesian Paired Samples T-Test")
		func:	"TTestBayesianPairedSamples"
	}
	Analysis
	{
		menu:	qsTr("One Sample T-Test")
		title:	qsTr("Bayesian One Sample T-Test")
		func:	"TTestBayesianOneSample"
	}
}
