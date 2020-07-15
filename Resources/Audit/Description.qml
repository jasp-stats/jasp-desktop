import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	title :				qsTr("Audit")
	name :				"Audit"
	description:		qsTr("JASP for Audit (JfA) is an officially supported add-on module that is designed specifically with the auditor in mind. JfA helps the auditor in interpreting, explaining, and reporting the analyses and leaves a transparent audit trail.")
	version:			"0.1"
	author:				"Koen Derks, Jacques de Swart, Eric-Jan Wagenmakers, Jan Wille & Ruud Wetzels"
	maintainer:			"Koen Derks <k.derks@nyenrode.nl>"
	website:			"www.github.com/koenderks/jfa"
	license:			"GPL (>= 2)"
	icon:				"audit-module.svg"

	GroupTitle
	{
		title:    		qsTr("Workflow")
		icon:			"audit-workflow.svg"
	}

	Analysis
	{
		title:			qsTr("Audit Workflow")
		func:			"auditClassicalWorkflow"
		requiresData:	true
	}

	Analysis
	{
		title:			qsTr("Bayesian Audit Workflow")
		func:			"auditBayesianWorkflow"
		requiresData:	true
	}

	GroupTitle
	{
		title:    		qsTr("Planning")
		icon:			"audit-planning.svg"
	}

	Analysis
	{
		title:			qsTr("Planning")
		func:			"auditClassicalPlanning"
		requiresData:	false
	}

	Analysis
	{
		title:			qsTr("Bayesian Planning")
		func:			"auditBayesianPlanning"
		requiresData:	false
	}

	GroupTitle
	{
		title:    		qsTr("Selection")
		icon:			"audit-selection.svg"
	}

	Analysis
	{
		title:			qsTr("Selection")
		func:			"auditSelection"
		requiresData:	true
	}

	GroupTitle
	{
		title:    		qsTr("Evaluation")
		icon:			"audit-estimation.svg"
	}

	Analysis
	{
		title:			qsTr("Evaluation")
		func:			"auditClassicalEvaluation"
		requiresData:	false
	}

	Analysis
	{
		title:			qsTr("Bayesian Evaluation")
		func:			"auditBayesianEvaluation"
		requiresData:	false
	}

		GroupTitle
	{
		title:    		qsTr("Other")
		icon:			"audit-estimation.svg"
	}

	Analysis
	{
		title:			qsTr("Benford's Law")
		func:			"auditClassicalBenfordsLaw"
		requiresData:	true
	}

	Analysis
	{
		title:			qsTr("Estimation")
		func:			"auditClassicalEstimation"
		requiresData:	true
	}
}
