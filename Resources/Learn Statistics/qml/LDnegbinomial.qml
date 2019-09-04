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
import JASP.Widgets 1.0

Form {
  id: form

  Section
  {
    expanded: true
    title: qsTr("Show Distribution")
    Group
    {
        Layout.columnSpan: 2
        DropDown
        {
            name: "parametrization"
            id:   parametrization
            indexDefaultValue: 0
            label: qsTr("Parameter")
            values: [
                { label: "φ, p",  value: "prob"},
                { label: "φ, μ",  value: "mean" }
              ]
            visible: true
        }

        Group
        {
            columns: 1
            DoubleField{ name: "size"; label: qsTr("φ"); id: size; defaultValue: 1; negativeValues: false }
            DoubleField
            { 
                name:  "par"; label: ["p", "μ"][parametrization.currentIndex]; id: par
                defaultValue: [0.5, 1][parametrization.currentIndex]
                min: 0; max: [1, Infinity][parametrization.currentIndex]
            }
        }

    }
    Group
    {
        title: qsTr("Display")
        CheckBox{ label: qsTr("Explanatory text"); name: "explanatoryText"}
        CheckBox{ label: qsTr("Parameters, support, and moments"); name: "parsSupportMoments" }
        CheckBox{ label: qsTr("Formulas"); name: "formulas"; visible: false}
        CheckBox{ label: qsTr("Probability mass function"); id: plotPMF; name: "plotPMF"; checked: true }
        CheckBox{ label: qsTr("Cumulative distribution function"); id: plotCMF; name: "plotCMF"; checked: false }
    }

    Group
    {
        title: qsTr("Options")
        enabled: plotPMF.checked || plotCMF.checked
        Group
        {
            title: qsTr("Range")
            columns: 2
            IntegerField
            { 
                name: "min_x"; label: qsTr("From"); id: min_x;
                defaultValue: 0; min: 0; max: max_x.value
            }
            IntegerField
            { 
                name: "max_x"; label: qsTr("to"); id: max_x;
                defaultValue: 5; min: min_x.value
            }
        }
        DoubleField{ name:  "range"; label: qsTr("Range"); defaultValue: 3; id: range; visible: false}
        Group
        {
            title: qsTr("Highlight")
            Group
            {
                columns: 2
                CheckBox{ name: "highlightDensity"; label: qsTr("Mass"); id: highlightDensity }
                CheckBox{ name: "highlightProbability"; label: qsTr("Cumulative Probability"); id: highlightProbability }
            }
            Group
            {
                columns: 2
                IntegerField
                { 
                    name: "min"; label: qsTr("Limits"); afterLabel: qsTr("≤ X ≤"); id: min;
                    negativeValues: false; defaultValue: 0; max: max.value
                }
                IntegerField
                { 
                    name: "max"; label: ""; id: max;
                    min: min.value; defaultValue: 5
                }
            }
        }

    }
  }
  
  Section
  {
      title: qsTr("Generate and Display Data")
      Group
      {
          title: qsTr("Generate new variable from NBinomial(φ = ") + size.value + [",p = ", ",μ = "][parametrization.currentIndex] + par.value + ")"
          AddColumnField{ name: "newVariableName"; text: "Variable name: "; fieldWidth: 120; placeholderText: "e.g., random nbinomial" }
          IntegerField{   name: "sampleSize"; label: "Number of samples: "; min: 1; defaultValue: 100 }
          Button{name: "simulateNowButton"; label: "Draw samples"; id: simulateNowButton; onClicked:{
            if (simulateNow.checked) simulateNow.checked = false; else simulateNow.checked = true
          }}
          CheckBox{name: "simulateNow"; visible: false; id: simulateNow}
      }
      VariablesForm
      {
          height: 100
          visible: true
          AvailableVariablesList { name: "allVariables" }
          AssignedVariablesList  { name: "variable"; label: qsTr("Get variable from data set"); allowedColumns: ["scale"]; singleVariable: true }
      }

      Group
      {
          title: qsTr("Statistics")
          CheckBox{ name: "summary"; label: qsTr("Descriptives") }
          CheckBox
          {
              name: "moments"; label: qsTr("First"); childrenOnSameRow: true
              IntegerField{name: "momentsUpTo"; afterLabel: qsTr("observed moments"); defaultValue: 2; min: 1; max: 10}
          }
      }

      Group
      {
          title: qsTr("Plots")
          CheckBox
          {
              name: "histogram";  label: qsTr("Histogram"); childrenOnSameRow: true
              //IntegerField{ name: "histogramBins"; afterLabel: qsTr("bins"); defaultValue: 30 }
          }
          CheckBox{ name: "ecdf"; label: qsTr("Empirical cumulative distribution") }
      }
  }

  Section
  {
      title: qsTr("Estimate Parameters")

      Group
      {
          CheckBox{ name: "methodMLE";      label: qsTr("Maximum likelihood"); visible: true  }
          CheckBox{ name: "methodMoments";  label: qsTr("Method of moments");  visible: false }
          CheckBox{ name: "methodUnbiased"; label: qsTr("Unbiased estimator"); visible: false }
      }

      Group
      {
          title: qsTr("Output")
          CheckBox
          {
              name: "outputEstimates"; label: qsTr("Estimates"); checked: true
              CheckBox{ name: "outputSE"; label: qsTr("Std. error"); checked: false }
              CheckBox
              {
                  name: "ciInterval"; label: qsTr("Confidence interval"); childrenOnSameRow: true
                  PercentField{ name: "ciIntervalInterval"; label: ""; defaultValue: 95 }
              }
          }

          CheckBox{ name: "outputVarCov"; label: qsTr("Variance-covariance"); checked: false; visible: false }
          CheckBox{ name: "outputCor";    label: qsTr("Correlation"); checked: false; visible: false }
      }
  }
  Section
  {
      title: qsTr("Assess Fit")

      Group
      {
          title: qsTr("Plots")
          columns: 2
          CheckBox{ name: "estPMF"; label: qsTr("Histogram vs. theoretical pmf") }
          CheckBox{ name: "qqplot"; label: qsTr("Q-Q plot")                      }
          CheckBox{ name: "estCDF"; label: qsTr("Empirical vs. theoretical cdf") }
          CheckBox{ name: "ppplot"; label: qsTr("P-P plot")                      }
      }

      Group
      {
          title: qsTr("Statistics")
          //CheckBox{ name: "kolmogorovSmirnov";  label: qsTr("Kolmogorov-Smirnov")}
          //CheckBox{ name: "cramerVonMisses";    label: qsTr("Cramér–von Mises")  }
          //CheckBox{ name: "andersonDarling";    label: qsTr("Anderson-Darling")  }
          //CheckBox{ name: "shapiroWilk";        label: qsTr("Shapiro-Wilk")      }
          CheckBox{ name: "chiSquare"; label: qsTr("Chi-square")}
      }

  }
}
