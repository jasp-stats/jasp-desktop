import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "Reliability"
	title		: qsTr("Reliability")
	description	: qsTr("This module offers Reliability analyses.")
	version		: "0.1"
	author		: "Julius M. Pfadt, Don van den Bergh & Eric-Jan Wagenmakers"
	maintainer	: "Julius M. Pfadt <julius.pfadt@gmail.com>"
	website		: "https://github.com/jasp-stats/Reliability"
	license		: "GPL (>= 2)"
	icon		: "reliability_icon_classic.svg"


	GroupTitle
	{
		title:	qsTr("Classical")
		icon:	"reliability_icon_classic.svg"
	}

	Analysis
	{
		title:	qsTr("Single-Test Reliability Analysis")
		qml: 	"ReliabilityFrequentist.qml"
		func: 	"reliabilityFrequentist"
	}

	Separator {}

	GroupTitle
	{
		title:	qsTr("Bayesian")
		icon:	"reliability_icon_bayesian.svg"
	}

	Analysis
	{
		menu: 	qsTr("Single-Test Reliability Analysis")
		title: 	qsTr("Bayesian Single-Test Reliability Analysis")
		qml: 	"ReliabilityBayesian.qml"
		func: 	"reliabilityBayesian"
	}
}
