Guide to writing an analysis interface in QML
=============================================

QML (Qt Modeling Language) is a user interface markup language that JASP uses to show the analysis input panel. In this panel the user can specify what options should be set to what values and thereby change the tables and plots that the analysis computes. QML is a very flexible language that allows us to easily generate checkboxes, dropdowns and other common interface components.  
To create a more uniform layout and make it easier to add new analyses we have provided a number of standardized components. These components should satisfy most analyses, and are explained in this document.  
One important pardigm in QML is containment: the implementation of a component is hidden, only public properties (specified by the component self) can be used to manipulate the component.  
QML uses JavaScript expressions to set values of these properties. Another important paradigm in QML is property binding: this means that when you set a property of a component with a JavaScript expression that uses values of other elements, whenever the values of those elements change, the property value will change automatically:
  ```qml
  CheckBox { id: addFrequencyTable; label: "Frequency tables" }
  IntegerField { label: "Maximum distinct values" ; enabled: addFrequencyTable.checked }
  ```
  In this example, the `enabled` property of the IntegerField is bound to the addFrequencyTable CheckBox `checked` property: whenever the `checked` property changes, the `enabled` property will change automatically. Here the expression `addFrequencyTable.checked` is simple, but it can be much more complex using several other propeties of other components.  
For more explanation on QML, you can read [wikipedia on QML](https://en.wikipedia.org/wiki/QML) or the [official site](https://doc.qt.io/qt-5/qtqml-index.html)  
To write a QML form you should follow the [styleguide](jasp-qml-style-example.qml).

Table of Contents:
- [Components](#components)
  * [General Input](#general-input)
    + [CheckBox](#checkbox)
    + [RadioButton](#radiobutton)
    + [DropDown](#dropdown)
    + [Slider](#slider)
    + [DoubleField](#doublefield)
    + [IntegerField](#integerfield)
    + [PercentField](#percentfield)
    + [CIField](#cifield)
    + [TextField](#textfield)
    + [FormulaField](#formulafield)
    + [TextArea](#textarea)
  * [Variable Specification](#variable-specification)
    + [AvailableVariablesList](#availablevariableslist)
    + [AssignedVariablesList](#assignedvariableslist)
	+ [FactorLevelList](#factorlevellist)
  * [Complex Components](#complex-components)
    + [ComponentsList](#componentslist)
    + [TabView](#tabview)
    + [InputListView](#inputlistview)
    + [TableView](#tableview)
  * [Grouping](#grouping)
    + [Group](#group)
    + [Section](#section)
- [Layout of Components](#layout-of-components)
    + [Layout.rowSpan](#layoutrowspan)
    + [Layout.columnSpan](#layoutcolumnspan)
- [Connecting Multiple Components](#connecting-multiple-components)
- [An Example](#an-example)
  * [1. Specifying Imports](#1-specifying-imports)
  * [2. Adding the Form](#2-adding-the-form)
  * [3. Adding the Components](#3-adding-the-components)
- [Advanced Usage](#advanced-usage)
- [Custom Imports](#custom-imports)


## Components
The components can roughly be divided in three classes. One that deals with general inputs (e.g., checkboxes), one that deals with assigning variables and one that groups components together. Each will be covered in the following section.
Some remarks about these components:
- They are all QML items, so they automatically get generic QML properties like `enabled` or `visible`.
- In several examples you may encounter `qsTr()`, it is important that this function wraps all text that will be shown in the interface. It will provide the possibility to translate JASP in the future.
- The components described below may generally be nested to an arbitrary level (e.g., a checkbox in a group in a checkbox).

All (well almost all) of these components have a property `name` that is also used in JASP-files to store the value the user selected. That means that whenever you change `name` for a component for a newer version of your analysis/module the stored value will be ignored. To make sure the user entered information isn't lost you can add an [upgrade.json to your module](jasp-upgrade-json.md).

### General Input
These components are quite common in questionnaires and input forms on websites, they include the checkbox, radiobutton, dropdown, slider and textfields where text may be entered.

#### CheckBox
Check button that can be toggled on and off. If some components are nested inside a CheckBox, they are automatically enabled or disabled when the CheckBox is set or not.
Properties
- `name`: string identifier (in your R code you will be able to retrieve the value of the checkbox through this identifier)
- `label`: text that will be shown to the left of the checkbox
- `checked`: [optional, default: `false`] boolean specifying if it should be set per default on or not.
- `childrenOnSameRow`: [optional, default: `false`] boolean specifying if components (e.g., other checkboxes, textfields) nested within the checkbox should be shown on the same row or, alternatively, below the checkbox
- `columns`: [optional, default: `1`] integer specifying how many columns the nested components should occupy (e.g., when set to 3, with three nested checkboxes, these three checkboxes will appear side by side on the same horizontal row)

<details>
	<summary>Examples</summary>

  ```qml
  CheckBox { name: "pearson"; label: qsTr("Pearson"); checked: true }
  ```
  ![Image example](/Docs/development/img/qml-guide/CheckBox_example_1.png)

  ```qml
  CheckBox
  {
    name: "homogeneityCorrections"
    label: qsTr("Homogeneity corrections")
    columns: 3
    CheckBox { name: "homogeneityNone";   label: qsTr("None")           ; checked: true }
    CheckBox { name: "homogeneityBrown";	label: qsTr("Brown-Forsythe") ; checked: true }
    CheckBox { name: "homogeneityWelch";	label: qsTr("Welch")          ; checked: true }
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/CheckBox_example_2.png)

</details>

#### RadioButton
A RadioButton is used with other RadioButton's inside a `RadioButtonGroup`. Contrary to CheckBox, only 1 RadioButton can be checked at the same time.

RadioButtonGroup properties:
- `name`: string identifier (in your R code you will be able to retrieve the value of the checked radiobutton through this identifier)
- `title`: text that is shown above the set of radiobuttons
- `radioButtonsOnSameRow`: [optional, default: `false`] per default, the RadioButton's are placed vertically under the title. Set this property to true to place the buttons horizontally.
- `columns`: [optional, default: `1`] integer specifying how many columns should be used to display the RadioButton's

RadioButton properties:
- `value`: the value of the `RadioButtonGroup` that is send to R when this radiobutton is checked
- `label`: the text that is shown to the right of the radiobutton
- `checked`: [optional, default: `false`] boolean specifying if it should contain a checkmark (and thus be the default); one radiobutton should have this property set to `true`
- `childrenOnSameRow`: [optional, default: `false`] boolean specifying if components (e.g., checkboxes, textfields) nested within the radiobutton should be shown on the same row or, alternatively, below the radiobutton
- `columns`: [optional, default: `1`] integer specifying how many columns the nested components should occupy (e.g., when set to 3, with three nested checkboxes, these three checkboxes will appear side by side on the same horizontal row)

<details>
	<summary>Examples</summary>

  ```qml
  RadioButtonGroup
  {
    name: "countProp"
    title: qsTr("Display")
    RadioButton { value: "descCounts"; label: qsTr("Counts"); checked: true }
    RadioButton { value: "descProps"; label: qsTr("Proportions") }
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/RadioButton_example_1.png)

  ```qml
  RadioButtonGroup
  {
    name: "steppingMethodCriteriaType"
    title: qsTr("Stepping Method Criteria")
    RadioButton
    {
      value: "usePValue"; label: qsTr("Use p value"); checked: true
      columns: 2
      DoubleField { name: "steppingMethodCriteriaPEntry";		label: qsTr("Entry");	fieldWidth: 60; defaultValue: 0.05; max: 1; decimals: 3 }
      DoubleField { name: "steppingMethodCriteriaPRemoval";	label: qsTr("Removal");	fieldWidth: 60; defaultValue: 0.1; max: 1; decimals: 3 }
    }
    RadioButton
    {
      value: "useFValue"; label: qsTr("Use F value")
      columns: 2
      DoubleField { name: "steppingMethodCriteriaFEntry";		label: qsTr("Entry");	fieldWidth: 60; defaultValue: 3.84; decimals: 3 }
      DoubleField { name: "steppingMethodCriteriaFRemoval";	label: qsTr("Removal");	fieldWidth: 60; defaultValue: 2.71; decimals: 3 }
    }
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/RadioButton_example_2.png)

</details>

#### DropDown
Properties
- `name`: string identifier (in your R code you will be able to retrieve the value of the dropdown through this identifier)
- `label`: [optional, default: `""`] text that will be shown above the dropdown
- `values`: [optional, default: empty] array of (named) values that is shown in the dropdown list, in the case of an unnamed array the value and the label are the same, but when a named array is provided the label shown to the user can be different from the value send to R (see example below)
- `startValue`: [optional, default: ``] string specifying the value of the dropdown item that is selected by default
- `currentIndex`, `currentValue`, `currentLabel`: [optional] gives the current index, value or label of the selected item. Can be used to set the current item.
- `currentColumnType`: [read-only] if the values of the dropdown are variables of the dataset, then this property gives the type of this variable, i.e. `scale`, `nominal`, `nominalText`, `ordinal`.
- `source`: [optional, default: empty] if `values` is empty, you can use the name of a VariablesList or other kind of source (see [Variable Specification](#variable-specification)) to set the values of the DropDown. If source is empty, then all available columns of the dataset are used.
- `addEmptyValue`: [optional, default: `false`] if `true`, add an empty value with a place holder (see `placeHolderText`) as first element of the dropdown list
- `placeHolderText`: [optional, default: `<no choice>`] name used if an ampty value is added in the dropdown list
- `addScrollBar`: [optional, default: `false`] add a scrollbar when the list is displayed.

<details>
	<summary>Examples</summary>

  ```qml
  DropDown
  {
    name: "orthogonalSelector"
    values: ["none", "varimax", "quartimax", "bentlerT", "equamax", "varimin"]
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/DropDown_example_1.png)

  ```qml
  DropDown
  {
    name: "sumOfSquares"
    indexDefaultValue: 2
    label: qsTr("Sum of squares")
    values:
    [
      { label: "Type \u2160", value: "type1"},
      { label: "Type \u2161", value: "type2"},
      { label: "Type \u2162", value: "type3"}
    ]
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/DropDown_example_2.png)

</details>

#### Slider
Slider is used to select a value by sliding a handle along a track.
Properties
- `name`: string identifier (in your R code you will be able to retrieve the value of the slider through this identifier)
- `label`: [optional, default: `""`] text that will be shown above the slider
- `value`: [optional, default: `0.5`] default value
- `min`: [optional, default: `0`] minimum value
- `max`: [optional, default: `1`] maximum value
- `vertical`: [optional, default: `true`] set whether the slider should be displayed vertically or horizontally
- `decimals`: [optional, default: `2`] set the number of decimals the value gets


<details>
	<summary>Examples</summary>

  ```qml
  Slider
  {
    name: "highlight"
    label: qsTr("Highlight")
    value: 0.4
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/Slider_example_1.png)

</details>


#### DoubleField
Properties
- `name`: string identifier (in your R code you will be able to retrieve the value of the field through this identifier)
- `label`: [optional, default: `""`] text that will be shown to the left of the field
- `afterLabel`: [optional, default: `""`] text that will be shown to the right of the field
- `defaultValue`: [optional, default: `0`] numeric specifying the default value shown; can be an integer or a decimal value, just be sure to use a dot and not a comma
- `negativeValues`: [optional, default: `false`] specifies if the user should be able to input negative values (works only when the `min` argument is omitted)
- `min`: [optional, default: `0` if negativeValues is `false`, otherwise `-Infinity`] numeric specifying the minimum value a user can enter
- `max`: [optional, default: `Infinity`] numeric specifying the maximum value a user can enter
- `inclusive`: [optional, default: `JASP.MinMax`, possible values: `JASP.MinMax`, `JASP.MinOnly`, `JASP.MaxOnly`, `JASP.None`] specify whether the `min` and `max` parameters are inclusive or not. For example if `min` is `1` and `inclusive` is `JASP.MinMax` or `JASP.MinOnly` then value `1` is allowed.
- `decimals`: [optional, default: `3`] integer specifying how many decimals the user can enter
- `fieldWidth`: [optional, default: `40`] in pixels how wide should the field be


<details>
	<summary>Examples</summary>

  ```qml
  DoubleField
  {
    name: "eigenValuesBox"
    label: qsTr("Eigenvalues above")
    defaultValue: 1
    decimals: 1
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/DoubleField_example_1.png)

  ```qml
  DoubleField
  {
    name: "priorFixedEffects"
    label: qsTr("r scale fixed effects")
    defaultValue: 0.5
    fieldWidth: 50
    max: 2
    decimals: 1
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/DoubleField_example_2.png)

</details>

#### IntegerField
Properties
- `name`: string identifier (in your R code you will be able to retrieve the value of the field through this identifier)
- `label`: [optional, default: `""`] text that will be shown to the left of the field
- `afterLabel`: [optional, default: `""`] text that will be shown to the right of the field
- `defaultValue`: [optional, default: `0`] integer specifying the default value shown
- `negativeValues`: [optional, default: `false`] specifies if the user should be able to input negative values (works only when the `min` argument is omitted)
- `min`: [optional, default: `0` if negativeValues is `false`, otherwise `-Infinity`] integer specifying the minimum value a user can enter
- `max`: [optional, default: `Infinity`] integer specifying the maximum value a user can enter
- `inclusive`: [optional, default: `yes`, possible values: `yes`, `minOnly`, `maxOnly`, `no`] specify whether the `min` and `max` parameters are inclusive or not. For example if `min` is `1` and `inclusive` is `yes` or `minOnly` then value `1` is allowed.
- `fieldWidth`: [optional, default: `40`] in pixels how wide should the field be

<details>
	<summary>Examples</summary>

  ```qml
  IntegerField
  {
    name: "fixedSamplesNumber"
    label: qsTr("No. samples")
    defaultValue: 10000
    fieldWidth: 60
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/IntegerField_example_1.png)

  ```qml
  IntegerField
  {
    name: "percentileValuesEqualGroupsNo"
    min: 1
    max: 1000
    defaultValue: 4
    afterLabel: qsTr(" equal groups")
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/IntegerField_example_2.png)

</details>

#### PercentField
Properties
- `name`: string identifier (in your R code you will be able to retrieve the value of the field through this identifier)
- `label`: text that will be shown to the left of the field
- `afterLabel`: [optional, default `%`] text that will be shown to the right of the field
- `defaultValue`: [optional, default: `50`] integer specifying the default percentage shown
- `fieldWidth`: [optional, default: `40`] in pixels how wide should the field be
- `decimals`: [optional, default: `0`] integer specifying how many decimals the user can enter

<details>
	<summary>Examples</summary>

  ```qml
  CIField { name: "estimatesPlotsCI"; label: qsTr("Confidence interval") }
  ```
  ![Image example](/Docs/development/img/qml-guide/PercentField_example_1.png)

</details>

#### CIField
Specialized component for Confident Interval Input field (with right default values)<br>
Properties:
- `name`: string identifier (in your R code you will be able to retrieve the value of the field through this identifier)
- `label`: [optional, default: `""`] text that will be shown to the left of the field
- `afterLabel`: [optional, default: `"%"`] text that will be shown to the right of the field
- `defaultValue`: [optional, default: `"95"`] default text before the user enters anything
- `placeholderText`: [optional, default: `""`] text shown as a placeholder until a user enters something, will not be send to R if left unchanged by the user (mutually exclusive with `defaultValue`)
- `fieldWidth`: [optional, default: `40`] in pixels how wide should the field be
- `decimals`: [optional, default: `1`] integer specifying how many decimals the user can enter

#### TextField
Properties:
- `name`: string identifier (in your R code you will be able to retrieve the value of the field through this identifier)
- `label`: [optional, default: `""`] text that will be shown to the left of the field
- `afterLabel`: [optional, default: `""`] text that will be shown to the right of the field
- `defaultValue`: [optional, default: `""`] default text before the user enters anything
- `placeholderText`: [optional, default: `""`] text shown as a placeholder until a user enters something, will not be send to R if left unchanged by the user (mutually exclusive with `defaultValue`)
- `fieldWidth`: [optional, default: `40`] in pixels how wide should the field be

<details>
	<summary>Examples</summary>

  ```qml
  TextField { name: "labelYAxis"; label: qsTr("Label y-axis"); fieldWidth: 200 }
  ```
  ![Image example](/Docs/development/img/qml-guide/TextField_example_1.png)

</details>

#### FormulaField
Specialized component that allows to enter an R expression that must gives an number, for example 1/3, pi or sin(30).
It has all properties of the TextField, and these ones:
- `realValue`: [read-only] gives the value of the formula.
- `min`: [optional, default `-Infinity`] minimum value that the result of the formula can give.
- `max`: [optional, default `Infinity`] maximum value that the result of the formula can give.
- `inclusive`: [optional, default: `yes`, possible values: `yes`, `minOnly`, `maxOnly`, `no`] specify whether the `min` and `max` parameters are inclusive or not. For example if `min` is `1` and `inclusive` is `yes` or `minOnly` then value `1` is allowed.
- `parseDefaultValue`: [optional, default `true`] if the default value is a string which is not a R expression, then this property should be set to `false`
- `multiple`: [optional, default `false`] If `true`, it gives the right to have a formula that gives several values.
- `realValues`: [read-only] when `multiple` is `true`, this gives all the results of the R expression in an array

#### TextArea
For an input with text that can have many lines, use this component. As an `Enter` just adds a new line, and thus does not finish the editing, in order to apply the text you entered, you need to type `Ctrl-Enter`.<br>
Properties:
- `title`: [optional, default: `""`] title displayed above the TextArea.
- `text`: [default: `''`] text enterd by the user. This can be used also to set the default text the user will see when opening the analysis.
- `textType`: [optional, default: `""`, values: `JASP.TextTypeLavaan`, `JASP.TextTypeJAGS`, `JASP.TextTypeRcode`, `JASP.TextTypeModel`, `JASP.TextTypeSource`]: this component is in fact often used in a specialized mode, specified by this property):<br>
  * `JASP.TextTypeLavaan`, `JASP.TextTypeJAGS` and `JASP.TextTypeRcode`: the TextArea is used for Lavaan, JAGS or R code: it gets automatically the right syntax check
  * `JASP.TextTypeModel`: the TextArea is used as R model.
  * `JASP.TextTypeSource`: The TextArea can be then used as source for a VariablesList: all unique strings separated by a separator (per default a new line, but can be change via property `separator` will be then the terms of the VariablesList.<br>
- `separator`, `separators`: [optional, default: `"\n"`] string or array of strings used to split the string of a `source` TextArea
- `applyScriptInfo`: [optional, default: `Ctrl + Enter to apply`] information given at the bottom-right of the TextArea.

### Variable Specification
Most analyses are performed on variables. JASP offers a few ways of visualizing these variables in the input form. The general idea is that you get a field of available variables (taken from the dataset) and then one or more fields that the variables can be dragged to. Variable fields should be wrapped in a `VariablesForm`. This makes it possible to automatically align multiple variable fields and add assign-buttons.

#### AvailableVariablesList
Properties
- `name`: identifier of the variables list, this is never send to R
- `label`: [optional, default: `""`] text that will be shown above the variable field
- `source`: [optional, default: `""`] this can be set to the `id` or the `name` (or a list of id or names) of one or more Variables Lists. If no source is specified, then all variables of the data file are used as source. To specify several sources, you need to use an array: `["source1", "source2"]`. If you want to specify what you want to read from the source, you can add extra attributes:<br>
  * `use` attribute: if you want to read the levels of a `singleVariable` Variables List source, type: `source: [{name: "splitby", use: "levels"}]`. If you want only some variables with some types, type: `[{name: "source", use: "type=nominal|ordinal"}]`
  * `discard` attribute: if a Variables List source is itself composed by several kinds of sources, you can discard one of them in this way: `source: [{ name: "modelTerms", discard: "covariates" }]`
  * `condition` attribute: if a Variables List source has some components, and you want to retrieve the variables whose components have some specific values, type: `[ { name: "contrasts", condition: "contrast.currentValue == 'custom'" } ]` where `contrast` is the name of a DropDown component (added in the `contrasts` Variables List). In this example only variables with contrast having `custom` as value will be read from the source `contrasts`.
  * `values` attribute: if you want to add specific values to the list, you can add them in this way `source: [ { values: ["one", "two"] }, "myvariables" ]`. Here the values `one` and `two` are prepend to the names of the variables of the Variables List `myvariables`. If you want to display a label (that can be translated) different from the value used in the analysis, use: `source: [ { values: [ { label: qsTr("One"), value: "one" }, { label: qsTr("Two"), value: "two" } ], "myvariables" ]` (qsTr is the function you need to use if you want the string to be translatable).
  * `rSource`: name of a R source. This source should be generated in R with the jaspQmlSource function.
  * if you want to display not the variable names of the source, but the values of some component of this list, use the syntax `name.component`. For example, if the source `myvariables` has a TextField named `field`, a Variables List with source `source: "myvariables.field"` will display all values of the TextField component in place of the variable names.<br>

- `values`: [optional, default: `""`] this is a shortcut: in place of writing `source: [ values: ["one", "two"] ]`, type `values: ["one", "two"]`. If you want to display labels different from the values used by the analysis, use the same syntax as in the `source` property. You can also use an integer n: in this case the values are just [1, 2, ... n]
- `rSource`: [optional, default: `""`] this is a shortcut: in place of writing `source: [ { rSource: "myRSource" } ]`, type `rSource: "myRSource"]`.
- `width`: [optional, default: 2/5 of the VariablesForm width] in pixels how wide should the field be
- `count`: [read-only integer] Gives the number of rows of the list.

Note: `height` should be defined on `VariablesForm` itself.

<details>
	<summary>Examples</summary>

  ```qml
  VariablesForm
  {
    AvailableVariablesList { name: "allVariables" }
    AssignedVariablesList  { name: "fixedFactors"; label: qsTr("Fixed Factors"); allowedColumns: ["ordinal", "nominal"] }
  }

  VariablesForm
  {
    height: 200
    AvailableVariablesList { name: "postHocTestsAvailable"; source: "fixedFactors"; rowComponent: CheckBox { name: "check" } }
    AssignedVariablesList {  name: "postHocTestsVariables" }
  }

   VariablesForm
  {
    height: 200
    AvailableVariablesList { name: "checkedPostHocAvailable"; source: [ name: "postHocTestsVariables", condition: "check.checked" ] }
    AssignedVariablesList { name: "checkedPostHoc" }
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/availableVariablesList_example_1_1.png)
  ![Image example](/Docs/development/img/qml-guide/availableVariablesList_example_1_2.png)

</details>

#### AssignedVariablesList
Properties
- `name`: identifier of the particular variable field (in your R code you will be able to retrieve the assigned variable(s) through this identifier)
- `label`: [optional, default: `""`] text that will be shown above the variable field
- `columns`: [optional, default: 1] number of columns of the list.
- `allowedColumns`: [optional, default: empty, possible values: `["scale", "ordinal", "nominal"]` ] array specifying the allowed column types
- `suggestedColumns`: [optional, default: empty, possible values: `["scale", "ordinal", "nominal"]` ] array specifying the suggested column types. These types will be displayed as icons at the bottom-right of the AssignedVariablesList. If `suggestedColumns` is empty and `allowedColumns` is specified, then `suggestedColumns` get automatically the value of `allowedColumns`. If `allowedColumns` is empty and `suggestedColumns` is specified, then the following rules apply:
    * `scale` allows Nominal Integer and Ordinal columns
    * `nominal` allows all Nominal columns (Integer or String), and Ordinal column
    * if no `suggestedColumns` and no `allowedColumns` is specified, then all types of columns are allowed
- `maxRows`: [optional, default: `-1`] maximum number of rows the list can accept. -1 means no limit.
- `singleVariable`: [optional, default: `false`] if true, set the maxRows to 1
- `listViewType`: [optional] enumerative that specifies the type of `AssignedVariablesList`, when omitted we get a normal list, options are `JASP.Layers` (see Contingency Tables), `JASP.Interaction` (see ANOVA) and `JASP.RepeatedMeasures` (see Repeated Measures ANOVA)
- `addInteractionsByDefault`: [optional, default: `true`] Specify if all interactions between factors should be automatically added to the model. Only has an effect if `listViewType == JASP.Interaction`.
- `width`: [optional, default: 2/5 of the VariablesForm width] in pixels how wide should the field be
- `height`: [optional] in pixels how heigh should the field be. Per default, it is set so that all AssignedVariablesList's fit the VariablesForm. If you set the height for 1 AssignedVariablesList, it will try to set height of the other AssignedVariablesLists's so that they all fit the heigth of the VariablesForm.
- `count`: [read-only integer] Gives the number of rows of the list.
- `rowComponentTitle`: [optional, default '']: title for rowComponent. It will be added at the right side above the list.
- `rowComponent`: It is possible to add one or more components for each assigned variable. To do this, add the `rowComponent` property. If you want to add just one extra component, a CheckBox for example, just add it here: <br>
    ```qml
    rowComponent:  CheckBox { name: "enableNumber"; checked: true }
	```
	
    If you want several components, then wrap it with a `Row` item, like this:
	```qml
	rowComponent: Row 
    {
        CheckBox { name: "enableNumber"; checked: true }
        TextField { name: "myField"; value: rowValue }
    }
	```
	![Image example](/Docs/development/img/qml-guide/RowComponents_example.png)

    You can use so-called context properties in the components:
    * rowIndex: gives the row number in the list
    * rowValue: gives the name of the variable in the same row
- `optionKey`: [optional, default: 'variable' (for Interaction type, default is 'components'] If there is no rowComponent, the values are just given as an array of strings. But if one or more components are specified, the values are more complex: it is also an array, but each element of the array contains the name of the variable with as key the value of this `optionKey`, and the values of each component with as key the name of the component. 

<details>
	<summary>Examples</summary>

  ```qml
        VariablesForm
        {
                AvailableVariablesList { name: "allVariables" }
                AssignedVariablesList
                {
                        name: "dependent"
                        label: qsTr("Dependent Variable")
                        allowedColumns: ["scale"]
                        singleVariable: true
                }
        }
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/AssignedVariablesList_example_1.png)

  ```qml
  VariablesForm
  {
    AvailableVariablesList { name: "allVariables" }
    AssignedVariablesList
    {
      name: "modelTerms"
      label: qsTr("Model terms")
      listViewType: JASP.Interaction

      rowComponentTitle: "Add to null model"
      rowComponent: CheckBox { name: "isNuisance" }
    }
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/AssignedVariablesList_example_2.png)

</details>

#### FactorLevelList
With this component, the user can set names of factors, and for each factor, the names of its levels. This is used in Repeated Measures ANOVA.
Properties
- `name`: identifier of the particular variable field (in your R code you will be able to retrieve the assigned variable(s) through this identifier)
- `label`: [optional, default: `""`] text that will be shown above the variable field
- `width`: [optional, default: ±`230`] in pixels how wide should the field be
- `height`: [optional, default: `350`] in pixels how heigh should the field be
- `factorName`: [optional, default: `Factor`] default name of the factor.
- `levelName`: [optional, default: `Level`] default name of the level.
- `minFactors`: [optional, default: `1`] minimum number of factors.
- `minLevels`: [optional, default: `2`] minimum numbe of levels.

<details>
	<summary>Examples</summary>

  ```qml
  VariablesForm
  {
	FactorLevelList
    {
	  name: "repeatedMeasuresFactors"
      label: qsTr("Repeated Measures Factors")
      height: 180
	  factorName: qsTr("RM Factor")
    }
    AssignedVariablesList
    {
      name: "repeatedMeasuresCells"
      label: qsTr("Repeated Measures Cells")
      allowedColumns: ["scale"]
      listViewType: JASP.MeasuresCells
      source: "repeatedMeasuresFactors"
      height: 140
    }
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/RepeatedMeasuresFactorsList_example_1.png)

</details>

### Complex components
ComponentsList, TabView, InputListView and TableView are complex components that allow to view a variable number of input or other components

#### ComponentsList
This allows to displays a list of components either from a source or from user input.
- `name`: identifier of this component (in your R code you will be able to retrieve the text value(s) through this identifier)
- `title`: [optional, default: `""`] text that will be shown above this component
- `rowComponent`: as for the Variables List, you can specify all the components that will be repeated for each row.
- `source`: [optional, default: empty] as for Variables List you can specify any kind of source (see [Variable Specification]). It a source is used, the number of rows will automatically depends on the number of elements of this source. You can then also use the `rowValue` context property if you want to use the value of the source in one of the component.
- `values`: [optional, default: empty] In place of a source, you can specify direcly a list of values or value/label list as in the DropDown component.
- `addItemManually`: [optional, default 'false' if `source` and `values` are empty, else 'true'] If this is true, then it displays a '+' icon so that the user can add new rows, and a 'X' icon to delete a row.
- `minimumItems`: [optional, default '0'] if `addItemManually` is true, then at least this number of rows will be dislayed. These rows do not get the 'X' icon.
- `maximumItems`: [optional, default '-1'] if `addItemManually` is true, then it tells the maximum number of rows it will displayed: if the maximum is reached, the '+' icon won't be displayed.
- `optionKey`: [optional, default: 'value'] The values of this component is an array of object: each object as for key the name of the components specified in rowComponent, the key of the source or of the value (in `values`) is the value of this `optionKey`.

#### TabView
This is in fact the same as the ComponentsList, but the elements are displayed in Tab View.

#### InputListView
This component is nearly the same as the ComponentsList, but here there is no source or '+' icon to add a new row, but the user has an Input field where he can specify the name of the new row.
Properties
- `name`: identifier of this component (in your R code you will be able to retrieve the text value(s) through this identifier)
- `title`: [optional, default: `""`] text that will be shown above this component
- `placeHolder`: [optional, default `"New Value"`] text that is displayed as long as the user did not give a value. When a value is given a new input is then automatically added.
- `defaultValues`: [optional, default empty array] array of strings setting the first value(s) if the input controls.
- `minRows`: [optional, default 0] Minimum of rows the list view should display. No delete icon will be then displayed to these rows.
- `inputComponent`: [optional, default a TextField component] Per default the input field is a TextField component, but you may change this by setting a DoubleField of an IntegerField component.
- `rowComponent`: As for AssignedVariablesList, it is possible to add other components.
- `rowComponentTitle`: [optional, default ''] titel used for rowComponent.
- `optionKey`: [optional, default `"value"`] when using rowComponent, the `name` property indicates the name to use to retrieve all values of this component. These values are specified per row, and each row has different columns. The names of the rowComponents are used to specify the value of each component. The `optionKey` speficies then the name of the value of the Input field.

<details>
	<summary>Example</summary>

  ```qml
  InputListView
  {
      name                  : "groupNames"
      title                 : qsTr("Group name")
      optionKey             : "group"
      defaultValues         : ["Group 1", "Group 2"]
      placeHolder           : qsTr("New Group")
      minimumItems          : 2
      rowComponentTitle     : "Group color"

      rowComponent 			: DropDown
                      		{
                             	name: "groupColors"
                             	values: ["red", "blue", "yellow", "green", "purple", "orange"]
                      		}
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/InputListView_Example.png)

</details>


#### TableView
This component presents the input fields in a Table format.

Properties
- `name`: identifier of this component (in your R code you will be able to retrieve the text value(s) through this identifier)
- `modelType`: [required, must be either `MultinomialChi2Model`, `JAGSDataInputModel`, `FilteredDataEntryModel` or `CustomContrasts`] Specify which kind of TableView is used.
- `colName`: [optional, default `data`]: name of the generated column when `modelType` is `ListModelFilteredDataEntry` or `CustomContrasts`
- `itemType`: [optional, default `string`, can be also `double` or `integer`]
- `source`: [optional, default `source of the values the table is based on`]

<details>
	<summary>Example</summary>

  ```qml
  TableView
  {
      modelType		: "MultinomialChi2Model"
	  inputType		: "double"
      source            : "factor"
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/TableView_Example.png)

</details>


### Grouping
In order to add more structure to the input panel you can group components together. There are 2 levels of grouping: Section and Group. A Section can hide a group of components under a button. A Group is a smaller logical unit.

#### Group
Properties
- `title`: text shown above the grouped input components
- `columns`: [optional, default: `1`] integer specifying how many columns the grouped components should occupy

<details>
	<summary>Examples</summary>

  ```qml
  Group
  {
    title: qsTr("Output")
    CheckBox
    {
      name: "effects"; label: qsTr("Effects")
      RadioButtonGroup
      {
        name: "effectsType"
        RadioButton { value: "allModels"; label: qsTr("Across all models"); checked: true }
        RadioButton { value: "matchedModels"; label: qsTr("Across matched models") }
      }
    }
    CheckBox { name: "descriptives"; label: qsTr("Descriptives") }
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/Group_example_1.png)

</details>

#### Section
A Section sets components (or groups of components) under a button. By clicking the button, you can hide or display these components.
Properties
- `title`: text shown in the button that controls the collapse of an entire section
- `columns`: [optional, default: `2`] integer specifying how many columns the grouped components should occupy

<details>
	<summary>Examples</summary>

  ```qml
  Section
  {
    title: qsTr("Advanced Options")

    RadioButtonGroup
    {
      title: qsTr("Missing Values")
      name: "missingValues"
      RadioButton { value: "excludeCasesListwise"; label: qsTr("Exclude cases listwise"); checked: true }
      RadioButton { value: "excludeCasesPairwise"; label: qsTr("Exclude cases pairwise") }
    }

    Group
    {
      title: qsTr("Confidence Interval")
      enabled: chronbach.checked
      CheckBox
      {
        name: "confAlpha"
        label: qsTr("Cronbach's α analytical")
        PercentField { name: "confAlphaLevel"; label: qsTr("Confidence"); defaultValue: 95 }
      }
    }
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/ExpanderButton_example_1.png)

</details>


## Layout of Components
By default JASP creates a two column grid that is filled row-wise. So each component you place in your QML file will be added to the input panel left to right, top to bottom. We will demonstrate what this means by the use of a table.

Imagine a simple table with two columns and two rows. We have three components (CheckBox A, B and C) and each must go in their own cell. When these components are added row-wise we get A in the top left cell, then next to it B and on its own row C:

|         | <column 1> | <column 2> |
|---------|------------|------------|
| <row 1> | CheckBox A | CheckBox B |
| <row 2> | CheckBox C |            |

#### Layout.rowSpan
The above works great, but it definitely helps that the checkboxes are the same size. We could also add a RadioButtonGroup that is much taller than a simple checkbox. If we again imagine three components (but this time CheckBox A and B and RadioButtonGroup A) that go in the table in the same way:

|         | <column 1> | <column 2>         |
|---------|------------|--------------------|
| <row 1> | CheckBox A | RadioButtonGroup A |
|         |            | RadioButtonGroup A |
| <row 2> | CheckBox B |                    |

We notice that there is a lot of space between CheckBox A and CheckBox B. The reason for this is that CheckBox B is placed in the next available row. Fortunately we can remedy this with the property `Layout.rowSpan`; all we need to do is set `Layout.rowSpan: 2` in RadioButtonGroup A. This tells the engine that the component spans two rows and therefore CheckBox B will fit snuggly under CheckBox A:

|         | <column 1> | <column 2>         |
|---------|------------|--------------------|
| <row 1> | CheckBox A | RadioButtonGroup A |
| <row 2> | CheckBox B | RadioButtonGroup A |


#### Layout.columnSpan
Now imagine we really wanted to put the RadioButtonGroup in the top left cell and the two checkboxes below it on the same height as one another:

|         | <column 1>         | <column 2> |
|---------|--------------------|------------|
| <row 1> | RadioButtonGroup A |            |
|         | RadioButtonGroup A |            |
| <row 2> | CheckBox A         | CheckBox B |

However, if we simply added the components we would end up with this situation:

|         | <column 1>         | <column 2> |
|---------|--------------------|------------|
| <row 1> | RadioButtonGroup A | CheckBox A |
|         | RadioButtonGroup A |            |
| <row 2> | CheckBox B         |            |

To accomplish this we can set `Layout.columnSpan: 2` on RadioButtonGroup A. This results in the following situation:

|         | <column 1>         | <column 2>         |
|---------|--------------------|--------------------|
| <row 1> | RadioButtonGroup A | RadioButtonGroup A |
|         | RadioButtonGroup A | RadioButtonGroup A |
| <row 2> | CheckBox A         | CheckBox B         |


## Connecting Multiple Components
In `AvailableVariablesList` we showed that through its `source` property, we can establish a link between two components. There are other ways of connecting components and they are not limited to a `VariableList` (although they are a little different, only a `VariableList` uses the `name` property). QML allows us to do this is by adding an `id` to one component and then referencing this `id` in a different component. Note that the `id` can be any string starting with a lowercase, but it must be unique.

An example is enabling a checkbox if one of two different checkboxes is checked.
<p><details>
	<summary>Implementation</summary>

  ```qml
  CheckBox { name: "checkboxA"; label: qsTr("Some label"), id: checkA}
  CheckBox { name: "checkboxB"; label: qsTr("Some label"), id: checkB}

  CheckBox { name: "checkboxC";	label: qsTr("Some Label too"); enabled: checkA.checked || checkB.checked }
  ```

</details></p>

Here we make use of a JavaScript expression to evaluate if either CheckBox A or CheckBox B has been checked and if this is the case we enable CheckBox C. This JavaScript expression will be automatically updated each time that the checked values of checkA and checkB changes.

Another example would be setting the visibility of some textfield to invisible if a checkbox is not checked.

<details>
	<summary>Implementation</summary>

  ```qml
  CheckBox
  {
    name: "checkboxA"
    label: qsTr("Some label")
    id: checkA
    TextField { name: "textfieldA"; afterlabel: qsTr("x"); visible: checkA.checked}

  }
  ```

</details>

Any property can be set with an expression. A title of a Section for example:
<details>
	<summary>Implementation</summary>

  ```qml

        DropDown
        {
                id: estimator
                name: "estimator"
                label: qsTr("Estimator")
                values: ["EBICglasso", "cor", "pcor", "IsingFit", "IsingSampler", "huge", "adalasso", "mgm"]
        }

        Section
        {
                title: qsTr("Analysis Options - ") + estimator.currentText
                ....
        }

  ```

</details>


## An Example
We'll create a simple analysis input panel to show the workflow.
Read first the Guide to adding a module in JASP. In this way, when you edit your QML file, the changes made will be automatically seen in JASP. You can play with the components and their properties, and see immediately the output in JASP.


### 1. Specifying Imports
We can begin actual work on the QML file, first we have to tell the engine where to find our resources. To do so, we add a number of imports to the top of our file.

<details>
	<summary>Code</summary>

  ```qml
  import QtQuick 2.11
  import QtQuick.Layouts 1.3
  import JASP.Controls 1.0
  import JASP.Widgets 1.0
  ```

  If you want to import QML components from another jasp module, you can! 
  Have a look at [custom imports](#custom-imports).

</details>


### 2. Adding the Form
At this point we add a `Form` which will hold all our input components:

<details>
	<summary>Code</summary>

  ```qml
  import QtQuick 2.11
  import QtQuick.Layouts 1.3
  import JASP.Controls 1.0
  import JASP.Widgets 1.0

  Form
  {

  }
  ```

</details>

### 3. Adding the Components
It's now a matter of mixing and matching the previously shown components to create a form to our liking. Of course, if something isn't quite possible, you can also use QML features that were not covered here.

<details>
	<summary>Code</summary>

  ```qml
  import QtQuick 2.11
  import QtQuick.Layouts 1.3
  import JASP.Controls 1.0
  import JASP.Widgets 1.0

  Form
  {
    VariablesForm
    {
      AssignedVariablesList { name: "variables"; allowedColumns: ["ordinal", "nominal"] }
    }

    RadioButtonGroup
    {
      Layout.rowSpan: 2
      name: "hypothesis"
      title: qsTr("Hypothesis")
      RadioButton { value: "notEqualToTestValue"; label: qsTr("≠ Test value"); checked: true }
      RadioButton { value: "greaterThanTestValue"; label: qsTr("> Test value") }
      RadioButton { value: "lessThanTestValue"; label: qsTr("< Test value") }
    }

    DoubleField { name: "testValue"; label: qsTr("Test value: "); defaultValue: 0.5 ; max: 1; decimals: 2}

    Group
    {
      title: qsTr("Plots")
      CheckBox
      {
        name: "descriptivesPlots"
        label: qsTr("Descriptive plots")
        PercentField { name: "descriptivesPlotsConfidenceInterval"; label: qsTr("Confidence Interval"); defaultValue: 95 }
      }
    }
  }
  ```
  ![Image example](/Docs/development/img/qml-guide/general_example.png)

</details>

## Advanced Usage
QML is a very flexible format, besides giving you access to all of the built-in components and our JASP-components that are detailed under [components](#components) you can also add your own components! If you add a qml-file to the [qml directory](#jasp-adding-module.md#qml), for instance `Example.qml`, then it will be treated as a component by all other files in the directory. Make sure to give the file an actual capital as first letter though, otherwise qml will not see it as a component. Then you can use it in another qml file simply by adding `Example { id: yourExampleComponent }` and set any of the properties the root-component of your component if desired.

This will give you the opportunity to create reusable parts, for instance, each of your analyses might share a common core. You could add this to a `Core.qml` and then this could be part of each analysis and be exactly the same everywhere. If you then also make sure to have a common function in R that uses the options specified in the `Core.qml`-component you can reuse that as well.

The possibilities here are rather extended and possibly even endless. See [the Qt QML tutorials](https://doc.qt.io/qt-5/qml-tutorial.html) or the [official documentation](https://doc.qt.io/qt-5/qtqml-index.html) for more information on this.


## Custom Imports
To allow other jasp modules to reuse your qml code you need to turn it into a "QML Module" and you do this by adding a textfile to you `inst` folder called `qmldir`.
This file specifies which qml-components your module makes available to other modules.

For instance, suppose you want to share the entire `Descriptives` analysisform with other modules you can create `jaspDescriptives/qmldir` and that should contain:
```
module <jaspDescriptives>
Descriptives 1.0 qml/Descriptives.qml
```

And then suppose you want to use it in any other module you can simply do `import jaspDescriptives 1.0` and use it directly as `Descriptives {}`.
