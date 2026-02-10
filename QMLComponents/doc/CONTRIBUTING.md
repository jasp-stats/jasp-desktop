# JASP QML Documentation Guide

This guide serves as the primary resource for maintaining and extending the documentation for JASP's QML controls. It defines the style, format, and technical requirements for QDoc comments embedded in the source code.

## Documentation Scope

All public QML components in `QMLComponents/components/JASP/Controls` should be documented.
As of Feb 2026, all 64 core controls are documented.

## Style Guide (v3)

The documentation is written for **module developers** who use JASP controls in their analysis forms. Use the standard QDoc format.

### Key Principles

1. **No internal C++ details** — Do not expose `BoundControlBase`, `CheckBoxBase`, or other C++ class names. Users only need to know the QML API.
2. **No `Serialization` line** — Serialization is an internal implementation detail.
3. **Use actual values** — Replace `jaspTheme.*` references with their resolved numeric defaults (e.g., `200` instead of `jaspTheme.textFieldWidth`).
4. **Inherited Properties section** — Use the heading `\section1 Inherited Properties`.
5. **Always include `info`** — The `info` property must appear in every Inherited Properties section. It is the primary mechanism for generating help documentation.

### QDoc Comment Templates

#### 1. Standard Component Template

Use this format for most components, placed **immediately above** the root element:

```qml
/*!
    \qmltype [ComponentName]
    \inqmlmodule JASP.Controls 1.0
    \brief [One-line description.]

    [Optional longer description.]

    \section1 R Binding

    \list
    \li \b{R Type:} [e.g., \c logical, \c character, list or character vector]
    \li \b{Default:} [e.g., \c FALSE, "" , [] (empty array)]
    \endlist

    \section1 Properties

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b [prop] ([type]) - [Description]. Default: [value].
    \endlist

    \section1 Inherited Properties

    \list
    \li \b enabled (bool) - Whether the control is interactive. Default: true.
    \li \b visible (bool) - Whether the control is visible. Default: true.
    \li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
    \li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
    \endlist

    \section1 Signals

    \list
    \li \b [signalName]([params]) - [Description].
    \endlist

    \section1 Example

    \qml
    [ComponentName] {
        name: "myOption"
        label: qsTr("My Label")
    }
    \endqml
*/
```

#### 2. Preset / Derived Component Template

For QML components that **extend another QML type** (e.g., `AssignedVariablesList` extends `VariablesList`) and only preset properties without adding new ones:

```qml
/*!
    \qmltype [ComponentName]
    \inqmlmodule JASP.Controls 1.0
    \brief [One-line description.]

    [Description explaining what the component extends and how.]

    \section1 R Binding

    \list
    \li \b{R Type:} [type]
    \li \b{Default:} [default]
    \endlist

    \section1 Inherited Properties from [ParentType]

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b title (string) - Title displayed above the list. Alias: label. Default: "".
    \li \b [other parent-type props] ...
    \endlist

    \section1 Other Inherited Properties

    \list
    \li \b enabled (bool) - Whether the control is interactive. Default: true.
    \li \b visible (bool) - Whether the control is visible. Default: true.
    \li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
    \li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
    \endlist

    \section1 Example

    \qml
    [ComponentName] {
        name: "myOption"
    }
    \endqml
*/
```

> **Note:** For layout-only controls (e.g., `Group`) that do not bind to R options, omit the `\section1 R Binding` section and use a `\note` to indicate it is layout-only.

## Technical Requirements

### QDoc Configuration

- **Comment Placement**: QDoc comments must be placed **immediately above** the QML type declaration, NOT before `import` statements.
- **Version Consistency**: Ensure `\inqmlmodule JASP.Controls 1.0` matches the `qmldir` version.
- **Root Element**: The QML parser requires the documented QML type to be defined in the file.

### Generating Documentation

See `README.md` in this directory for instructions on running `qdoc` and `pandoc`.

## Reference: C++ Class Hierarchy & Types

When documenting, you may need to know the C++ base class to understand available properties (though you should not expose the C++ class name in the public docs).

```
JASPControl (jaspcontrol.h) ← Base for ALL controls
├── JASPListControl (jasplistcontrol.h)
│   ├── ComboBoxBase        → DropDown
│   ├── VariablesListBase   → VariablesList, AssignedVariablesList, etc.
│   ├── TableViewBase       → TableView, SimpleTableView, etc.
│   ├── TextAreaBase        → TextArea, JAGSTextArea
│   ├── ComponentsListBase  → ComponentsList
│   ├── FactorsFormBase     → FactorsForm
│   ├── FactorLevelListBase → FactorLevelList
│   └── InputListBase       → InputListView
├── CheckBoxBase            → CheckBox, Switch
├── RadioButtonsGroupBase   → RadioButtonGroup
├── RadioButtonBase         → RadioButton (child of group)
├── TextInputBase           → TextField, IntegerField, DoubleField, PercentField, etc.
├── SliderBase              → Slider
├── GroupBoxBase            → Group
├── ExpanderButtonBase      → Section
└── VariablesFormBase       → VariablesForm (layout only)
```

### Common Enum Types

| Enum | Location | Values |
| :--- | :------- | :----- |
| `TextInputType` | `textinputbase.h` | `IntegerInputType`, `StringInputType`, `NumberInputType`, `PercentInputType`, `DoubleArrayInputType`, `ComputedColumnType`, `FormulaType` |
| `ListViewType` | `jaspcontrol.h` | `AssignedVariables`, `Interaction`, `AvailableVariables`, `RepeatedMeasures`, `Layers`, `MultiFactor` |
| `ModelType` | `tableviewbase.h` | `Simple`, `MultinomialChi2`, `JAGSDataInput`, `Contrasts`, `Filtered`, `GridInput` |
