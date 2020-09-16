import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	title :         qsTr("Visual Modeling")
	name :          "Visual Modeling"
	description:    qsTr("Graphically-Based Data Analysis")
	version:        "0.0.1"
	author:         "Dustin Fife"
	icon:           "FlexplotLogo.svg"
	maintainer:     "Dustin Fife <fife.dustin@gmail.com>"
	website:        "github.com/dustinfife/flexplot"
	license:        "GPL (>= 2)"

	Analysis
	{
		title: 	qsTr("Flexplot")
		qml:   	"Flexplot.qml"
		func:	"flexplot"
	}

	Analysis
	{
		title:	qsTr("Linear Modeling")
		qml:	"linmod.qml"
		func:	"linmod"
	}

	Analysis
	{
		title:	qsTr("Mixed Modeling")
		qml:	"mixedmod.qml"
		func:	"mixedmod"
	}

	Analysis
	{
		title:	qsTr("Generalized Linear Modeling")
		qml:	"glinmod.qml"
		func:	"glinmod"
	}
}
