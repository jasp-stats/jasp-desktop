# JASP QML Controls

## Introduction

Welcome to the JASP QML Controls reference documentation. This library contains the custom controls used to build JASP analysis modules.

## Getting Started

To use these controls, import the module in your QML file:

```qml
import JASP.Controls 1.0
```

## All Controls

|  |  |
|----|----|
| [AddColumnField](qml-jasp-controls-addcolumnfield.md) | A text input field that creates a new computed column in the dataset |
| [AssignedPairsVariablesList](qml-jasp-controls-assignedpairsvariableslist.md) | A variable list preset for paired variable assignment |
| [AssignedRepeatedMeasuresCells](qml-jasp-controls-assignedrepeatedmeasurescells.md) | A variable list preset for assigning variables to repeated measures cells |
| [AssignedVariablesList](qml-jasp-controls-assignedvariableslist.md) | A variable list preset for assigned variable selection |
| [AvailableVariablesList](qml-jasp-controls-availablevariableslist.md) | Source list showing all available dataset variables |
| [BasicThreeButtonTableView](qml-jasp-controls-basicthreebuttontableview.md) | A table input control with Add, Delete, and Reset buttons |
| [BayesFactorType](qml-jasp-controls-bayesfactortype.md) | A pre-built radio button group for selecting Bayes Factor reporting format |
| [Button](qml-jasp-controls-button.md) | A generic clickable button |
| [CIField](qml-jasp-controls-cifield.md) | A percentage input field preset for confidence interval width |
| [CheckBox](qml-jasp-controls-checkbox.md) | A boolean toggle control that binds a true/false value to an R option |
| [CheckColumnIsFreeOrMineField](qml-jasp-controls-checkcolumnisfreeorminefield.md) | A text field that validates whether a column name is free or owned by the current analysis |
| [Chi2TestTableView](qml-jasp-controls-chi2testtableview.md) | A table view preset for entering multinomial chi-squared hypotheses |
| [ColorPalette](qml-jasp-controls-colorpalette.md) | A dropdown preset for selecting a color palette for plots |
| [ColumnLayout](qml-jasp-controls-columnlayout.md) | A vertical layout container with JASP-themed spacing |
| [ComponentsList](qml-jasp-controls-componentslist.md) | A dynamic list that repeats a user-defined component for each row |
| [ComputedColumnField](qml-jasp-controls-computedcolumnfield.md) | A text field for entering a computed column name |
| [ContrastsList](qml-jasp-controls-contrastslist.md) | A composite control for specifying contrasts for factor variables |
| [ControlErrorMessage](qml-jasp-controls-controlerrormessage.md) | An internal popup that displays validation errors or warnings above a control |
| [CustomContrastsTableView](qml-jasp-controls-customcontraststableview.md) | A table view preset for entering custom contrast weight matrices |
| [Divider](qml-jasp-controls-divider.md) | A horizontal line separator with an optional centered label |
| [DoubleField](qml-jasp-controls-doublefield.md) | A text field preset for entering numeric (double) values |
| [DropDown](qml-jasp-controls-dropdown.md) | A selection control that presents a list of options in a dropdown menu |
| [FactorLevelList](qml-jasp-controls-factorlevellist.md) | A grid editor for defining factors and their levels |
| [FactorsForm](qml-jasp-controls-factorsform.md) | A form for defining latent factors by assigning observed variables |
| [FactorsList](qml-jasp-controls-factorslist.md) | An assigned variables list with an editable title, used for individual factor panels |
| [FileSelector](qml-jasp-controls-fileselector.md) | A text field with a browse button for selecting files or directories |
| [Form](qml-jasp-controls-form.md) | Top-level container for an analysis input form |
| [FormulaField](qml-jasp-controls-formulafield.md) | A text field preset for entering R-style formulas |
| [GridLayout](qml-jasp-controls-gridlayout.md) | A responsive grid layout with JASP-themed spacing |
| [Group](qml-jasp-controls-group.md) | A layout container that groups related controls together |
| [InputListView](qml-jasp-controls-inputlistview.md) | An editable scrollable list for entering free-text values |
| [IntegerField](qml-jasp-controls-integerfield.md) | A text field preset for entering integer values |
| [JAGSTextArea](qml-jasp-controls-jagstextarea.md) | A text area preset for writing JAGS model code |
| [JagsTableView](qml-jasp-controls-jagstableview.md) | A table view preset for entering JAGS data |
| [Label](qml-jasp-controls-label.md) | A JASP-themed text label |
| [ModelTermsList](qml-jasp-controls-modeltermslist.md) | A preset VariablesList for building ANOVA model terms |
| [PercentField](qml-jasp-controls-percentfield.md) | A numeric field preset for entering percentage values (0–100) |
| [RadioButton](qml-jasp-controls-radiobutton.md) | A radio button option within a RadioButtonGroup |
| [RadioButtonGroup](qml-jasp-controls-radiobuttongroup.md) | A group of mutually exclusive radio button options |
| [RowLayout](qml-jasp-controls-rowlayout.md) | A horizontal layout with JASP-themed spacing |
| [Section](qml-jasp-controls-section.md) | A collapsible panel that groups child controls under a clickable header |
| [SetSeed](qml-jasp-controls-setseed.md) | A preset "Repeatability" group with a seed checkbox and integer field |
| [SimpleTableView](qml-jasp-controls-simpletableview.md) | A table view preset with Add Column, Delete Column, and Reset buttons |
| [Slider](qml-jasp-controls-slider.md) | A slider with a linked numeric text field |
| [SubjectivePriors](qml-jasp-controls-subjectivepriors.md) | A preset "Prior" section with default and informed prior distribution options |
| [Switch](qml-jasp-controls-switch.md) | A toggle switch control |
| [TabView](qml-jasp-controls-tabview.md) | A tabbed container that manages dynamic panels |
| [TableView](qml-jasp-controls-tableview.md) | A scrollable, editable data table for entering structured values |
| [Text](qml-jasp-controls-text.md) | A JASP-themed text display element |
| [TextArea](qml-jasp-controls-textarea.md) | A multi-line text input with optional line numbers and syntax highlighting |
| [TextField](qml-jasp-controls-textfield.md) | A single-line text input control for entering strings |
| [VariablesForm](qml-jasp-controls-variablesform.md) | A two-column layout with available variables on the left and assigned lists on the right |
| [VariablesList](qml-jasp-controls-variableslist.md) | Primary variable selection control in JASP |
