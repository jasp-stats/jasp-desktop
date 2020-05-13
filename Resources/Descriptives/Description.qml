import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "Descriptives"
	title		: qsTr("Descriptives")
	description	: qsTr("This module offers the Descriptives and Reliability analyses.")
	version		: "0.13"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"
	icon		: "analysis-descriptives.svg"

	Analysis
	{
		title:	qsTr("Descriptive Statistics")
		func:	"Descriptives"
	}

	Analysis
	{
		title:	qsTr("Reliability Analysis")
		func:	"ReliabilityAnalysis"
	}
}
