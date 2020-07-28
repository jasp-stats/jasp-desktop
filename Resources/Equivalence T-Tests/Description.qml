import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	title:			qsTr("Equivalence T-Tests (Beta)")
	name:			"Equivalence T-Tests"
	description:	qsTr("This is a test version of Equivalence Testing module.")
	version:		"0.13"
	author:			"Jill de Ron"
	maintainer:		"Jill de Ron <jillderon93@gmail.com>"
	website:		"https://jasp-stats.org"
	license:		"GPL (>= 2)"
	icon:			"equivalence-module.svg"

	Package { name: "TOSTER"	}
	Package { name: "metaBMA"; version: "0.6.2"	}

	Analysis
	{
		title:	qsTr("Equivalence Independent Samples T-Test")
		menu:	qsTr("Independent Samples T-Test")
		qml:	"EquivalenceIndependentSamplesTTest.qml"
		func:	"EquivalenceIndependentSamplesTTest"
	}
	Analysis
	{
		title:	qsTr("Equivalence Paired Samples T-Test")
		menu:	qsTr("Paired Samples T-Test")
		qml:	"EquivalencePairedSamplesTTest.qml"
		func:	"EquivalencePairedSamplesTTest"
	}
	Analysis
	{
		title:	qsTr("Equivalence One Sample T-Test")
		menu:	qsTr("One Sample T-Test")
		qml:	"EquivalenceOneSampleTTest.qml"
		func:	"EquivalenceOneSampleTTest"
	}

	Separator {}

	Analysis
	{
		title:	qsTr("Equivalence Bayesian Independent Samples T-Test")
		menu:	qsTr("Bayesian Independent Samples T-Test")
		qml:	"EquivalenceBayesianIndependentSamplesTTest.qml"
		func:	"EquivalenceBayesianIndependentSamplesTTest"
	}

	Analysis
	{
		title:	qsTr("Equivalence Bayesian Paired Samples T-Test")
		menu:	qsTr("Bayesian Paired Samples T-Test")
		qml:	"EquivalenceBayesianPairedSamplesTTest.qml"
		func:	"EquivalenceBayesianPairedSamplesTTest"
	}

	Analysis
	{
		title:	qsTr("Equivalence Bayesian One Sample T-Test")
		menu:	qsTr("Bayesian One Sample T-Test")
		qml:	"EquivalenceBayesianOneSampleTTest.qml"
		func:	"EquivalenceBayesianOneSampleTTest"
	}
}
