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
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0

Section
{
	property string	distributionType		: "continuous" // "counts" or "categorical"
	property bool includeShapiroWilk		: false

	title: enabled ? qsTr("Assess Fit") : qsTr("Assess Fit") + " - " + qsTr("[requires a loaded data set]")

	Group
	{
		title: qsTr("Plots")
		columns: 2
		CheckBox{ name: distributionType == "continuous" ? "estPDF" : "estPMF"; label: distributionType == "continuous" ? qsTr("Histogram vs. theoretical pdf") : qsTr("Histogram vs. theoretical pmf") }
		CheckBox{ name: "qqplot"; label: qsTr("Q-Q plot")                      }
		CheckBox{ name: "estCDF"; label: qsTr("Empirical vs. theoretical cdf") }
		CheckBox{ name: "ppplot"; label: qsTr("P-P plot")                      }
	}

	Loader
	{
		sourceComponent: distributionType == "continuous" ? continuous : ( distributionType == "counts" ? counts : categorical )
		Component
		{
			id: continuous
			Group
			{
				title: qsTr("Statistics")
				CheckBox{ name: "kolmogorovSmirnov"; label: qsTr("Kolmogorov-Smirnov")}
				CheckBox{ name: "cramerVonMisses";   label: qsTr("Cramér–von Mises")  }
				CheckBox{ name: "andersonDarling";   label: qsTr("Anderson-Darling")  }
				CheckBox{ name: "shapiroWilk";       label: qsTr("Shapiro-Wilk");	visible: includeShapiroWilk }
			}
		}

		Component
		{
			id: counts
			Group
			{
				title: qsTr("Statistics")
				CheckBox{ name: "chiSquare"; label: qsTr("Chi-square")}
			}
		}

		Component
		{
			id: categorical // for stuff like bernoulli / categorical dist (in future) we don't have any statistics implemented yet
			Group{ }
		}
	}
}
