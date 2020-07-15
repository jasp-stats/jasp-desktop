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
		
	GroupTitle
	{
		title: 	"Classical"
		icon:	"MixedModels_classical.svg"
	}

	Analysis
	{
		title:	"Linear Mixed Models"
		func:	"MixedModelsLMM"
	}

	Analysis
	{
		title:	"Generalized Linear Mixed Models"
		func:	"MixedModelsGLMM"
	}

	GroupTitle
	{
		title: 	"Bayesian"
		icon:	"MixedModels_bayesian.svg"
	}

	Analysis
	{
		menu:	"Linear Mixed Models"
		title:	"Bayesian Linear Mixed Models"
		func:	"MixedModelsBLMM"
	}

	Analysis
	{
		menu:	"Generalized Linear Mixed Models"
		title:	"Bayesian Generalized Linear Mixed Models"
		func:	"MixedModelsBGLMM"
	}
}
