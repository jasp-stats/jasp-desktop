import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name:			"Learn Bayes"
	title : 		qsTr("Learn Bayes")
	description:	qsTr("Learning Bayesian Statistics with JASP")
	icon:			"learn-bayes.svg"
	version:		"0.14"
	requiresData:	false
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"jasp-stats.org"
	license:		"GPL (>= 2)"

	GroupTitle
	{
		title:	qsTr("Counts")
		icon:	"analysis-bayesian-crosstabs.svg"
	}

	Analysis
	{
		title:	qsTr("Binomial Estimation")
		func:	"LSbinomialestimation"
	}

	Analysis
	{
		title:	qsTr("Binomial Testing")
		func:	"LSbinomialtesting"
	}
}
