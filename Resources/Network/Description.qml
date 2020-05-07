import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name:			"Network"
	title:			qsTr("Network")
	description:	qsTr("Network module.")
	icon:			"analysis-network.svg"
	version:		"0.13"
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"jasp-stats.org"
	license:		"GPL (>= 2)"
		

	Analysis
	{
		title:	qsTr("Network")
		func:	"NetworkAnalysis"
	}
}
