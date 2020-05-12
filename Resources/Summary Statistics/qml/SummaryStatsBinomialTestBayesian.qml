//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0


Form 
{
	info: qsTr(
`This function computes the Bayes factor for a binomially distributed observation.
The Bayesian binomial test is described in Jeffreys (1961, p. 256).
This test informs us whether the data support or contradict a value suggested for the parameter (chance) in question.

- Null model: *p = p0*
- Alt  model: *p ~ Beta(a,b)*
*p0* is the suggested value of the rate parameter of Binomial under null hypothesis. 
Data observed: *s* successes and *f* failures, total number of trials, *n = s + f*. 
In Theory of Probability, Jeffreys assumes a uniform prior on the rate parameter under alternative hypothesis. 
The Bayes factor used here is a more general case, assuming a beta prior on the rate parameter. 
*Note*: beta(1,1) corresponds to a uniform prior.`
);

	Group
	{
		IntegerField { name: "successes";	label: qsTr("Successes")	}
		IntegerField { name: "failures";	label: qsTr("Failures")		}
		FormulaField { name: "testValue";	label: qsTr("Test value"); defaultValue: "0.5" ; max: 1 }
    }

    Divider { }

	RadioButtonGroup
	{
		title: qsTr("Alt. Hypothesis")
		name: "hypothesis"
		RadioButton { value: "notEqualToTestValue";		label: qsTr("\u2260 Test value"); checked: true;	info: qsTr("Two-sided alternative hypothesis that the population mean is not equal to the test value.");	}
		RadioButton { value: "greaterThanTestValue";	label: qsTr("> Test value");						info: qsTr("One-sided alternative hypothesis that the population mean is larger than the test value.");		}
		RadioButton { value: "lessThanTestValue";		label: qsTr("< Test value");						info: qsTr("One sided alternative hypothesis that the population mean is smaller than the test value.");	}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "plotPriorAndPosterior";		label: qsTr("Prior and posterior")

			info: qsTr(`Displays the prior (dashed line) and posterior (solid line) density of the effect size under the alternative hypothesis; 
			the gray circles represent the height of the prior and the posterior density at effect size delta = 0. 
			The horizontal solid line represents the width of the 95% credible interval of the posterior.`)

			CheckBox 
			{ 
				name: "plotPriorAndPosteriorAdditionalInfo"; label: qsTr("Additional info"); checked: true 
				info: qsTr("Displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior density.")
			}
		}
	}

	BayesFactorType { }


	Group
	{
		title: qsTr("Prior")
		info:  qsTr("Parameters *a* and *b* are set to '1' each. This corresponds to a uniform prior.");

		DoubleField { name: "betaPriorParamA"; label: qsTr("Beta prior: parameter a"); defaultValue: 1; max: 10000; inclusive: JASP.None; decimals: 3 }
		DoubleField { name: "betaPriorParamB"; label: qsTr("Beta prior: parameter b"); defaultValue: 1; max: 10000; inclusive: JASP.None; decimals: 3 }
	}
}
