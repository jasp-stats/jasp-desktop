import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name:			"SEM"
	title : 		qsTr("SEM")
	description:	qsTr("SEM module.")
	icon:			"sem-latreg.svg"
	version:		"0.13"
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"jasp-stats.org"
	license:		"GPL (>= 2)"

	Analysis
	{
		title:	qsTr("Structural Equation Modeling")
		qml:	"SEMSimple.qml"
		func:	"SEMSimple"
	}

	Analysis
	{
		title:	qsTr("Mediation Analysis")
		qml:	"MediationAnalysis.qml"
		func:	"MediationAnalysis"
	}
}
