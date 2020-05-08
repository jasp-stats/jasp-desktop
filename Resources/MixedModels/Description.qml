import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	title		: "Mixed Models"
	icon		: "analysis-descriptives.svg"
	description	: "This module offers the Linear Mixed Models analysis."
	version		: "0.13"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"
		
	Analysis
	{
		title:	"Linear Mixed Models"
		func:	"LinearMixedModels"
	}
}
