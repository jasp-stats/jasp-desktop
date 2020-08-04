import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "Factor"
	title		: qsTr("Factor")
	icon		: "analysis-classical-sem.svg"
	description	: qsTr("This module offers the Principal Component, Confirmatory and Exploratory Factor analyses.")
	version		: "0.13"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"

	Analysis
	{
		title:	qsTr("Principal Component Analysis")
		func:	"PrincipalComponentAnalysis"
	}

	Analysis
	{
		title:	qsTr("Exploratory Factor Analysis")
		func:	"ExploratoryFactorAnalysis"
	}

	Analysis
	{
		title:	qsTr("Confirmatory Factor Analysis")
		func:	"ConfirmatoryFactorAnalysis"
	}
}
