import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	title :			qsTr("Distributions")
	name:			"Discover Distributions"
	icon:			"discoverdistributions-distributions.svg"
	description: 	qsTr("Discover distributions with JASP")
	version:		"0.1"
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"www.jasp-stats.org"
	license:		"GPL (>= 2)"

	Package	{ name:	"goftest"		}
	Package	{ name:	"fitdistrplus"	}
	Package	{ name:	"invgamma"		}

	GroupTitle
	{
		title: 	qsTr("Continuous")
		icon: 	"discoverdistributions-continuousDist.svg"
	}

	Analysis
	{
		title:	qsTr("Normal")
		qml:	"LDgaussianunivariate.qml"
		func:	"LDgaussianunivariate"
	}

	Analysis
	{
	  title:	qsTr("Student's t")
	  qml:		"LDt.qml"
	  func:		"LDt"
	}

	Analysis
	{
	  title:	qsTr("F-distribution")
	  qml:		"LDf.qml"
	  func:		"LDf"
	}

	Analysis
	{
		title:	qsTr("Chi-squared")
		qml:	"LDchisq.qml"
		func:	"LDchisq"
	}

	Analysis
	{
		title:	qsTr("Beta")
		qml:	"LDbeta.qml"
		func:	"LDbeta"
	}

	Analysis
	{
		title:	qsTr("Gamma")
		qml:	"LDgamma.qml"
		func:	"LDgamma"
	}

	Analysis
	{
		title:	qsTr("Inverse gamma")
		qml:	"LDgammaInverse.qml"
		func:	"LDgammaInverse"
	}

	Analysis
	{
		title:	qsTr("Exponential")
		qml:	"LDexponential.qml"
		func:	"LDexponential"
	}

	GroupTitle
	{
		title: 	qsTr("Discrete")
		icon: 	"discoverdistributions-discreteDist.svg"
	}

	Analysis
	{
		title:	qsTr("Bernoulli")
		qml:	"LDbernoulli.qml"
		func:	"LDbernoulli"
	}

	Analysis
	{
		title:	qsTr("Binomial")
		qml:	"LDbinomial.qml"
		func:	"LDbinomial"
	}

	Analysis
	{
		title:	qsTr("Negative binomial")
		qml:	"LDnegbinomial.qml"
		func:	"LDnegbinomial"
	}

	Analysis
	{
		title:	qsTr("Poisson")
		qml:	"LDpoisson.qml"
		func:	"LDpoisson"
	}
}
