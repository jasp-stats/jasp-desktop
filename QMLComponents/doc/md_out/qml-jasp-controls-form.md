<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

Form

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Inherited Properties](#inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# Form QML Type

The top-level container for an analysis input form. [More...](#details)

<div class="table">

|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-form-members.md)

## Detailed Description

It provides the standard layout for a JASP analysis: error/warning
message boxes, an optional R syntax panel, and a
<a href="qml-jasp-controls-gridlayout.md"
translate="no">GridLayout</a> content area where child controls are
placed. Every analysis QML file uses Form as its root element.

<div class="admonition note">

**Note:** Module developers should always use Form as the root element
of their analysis QML files.

</div>

## Properties

- **columns** (int) - Number of columns in the form's
  <a href="qml-jasp-controls-gridlayout.md"
  translate="no">GridLayout</a>. Default: 2.
- **plotWidth** (int) - Default width for plots in this analysis.
  Default: 480.
- **plotHeight** (int) - Default height for plots in this analysis.
  Default: 320.
- **majorVersion** (int) - Major version of the form. Default: 1.
- **minorVersion** (int) - Minor version of the form. Default: 0.
- **runAnalysisWhenOptionChange** (bool) - Re-run analysis on any option
  change. Default: true.

## Inherited Properties

- **enabled** (bool) - Whether the control is interactive. Default:
  true.
- **visible** (bool) - Whether the control is visible. Default: true.

## Example

``` qml
Form {
    VariablesForm {
        AvailableVariablesList { name: "allVariables" }
        AssignedVariablesList  { name: "dependent"; singleVariable: true }
    }
}
```
