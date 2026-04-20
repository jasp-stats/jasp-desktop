[JASP.Controls](jasp-controls-qmlmodule.md)

Section


# Section QML Type

A collapsible panel that groups child controls under a clickable header.


|                   |                                                   |
|-------------------|---------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                        |
| Inherited By:     | [SubjectivePriors](qml-jasp-controls-subjectivepriors.md)                |


- [List of all members, including inherited
  members](qml-jasp-controls-section-members.md)

## Detailed Description

Displays a titled expander bar with an arrow icon. Clicking the header
toggles visibility of the child controls area with an animated
transition. Commonly used to organize optional or advanced settings.


**Note:** Section does not bind to R options. It is a layout-only
control.


## Properties

- **title** (string) - Text displayed in the expander header. Alias:
  text. Default: "".
- **expanded** (bool) - Whether the section content is visible. Default:
  false.
- **columns** (int) - Number of columns in the content
  [GridLayout](qml-jasp-controls-gridlayout.md). Default: 2.
- **spacing** (real) - Row spacing in the content area. Default:
  jaspTheme.rowGridSpacing.
- **info** (string) - Info used for tooltips and help generation.
  Default: "".

## Example

``` qml
Section {
    title: qsTr("Advanced Options")
    expanded: false

    CheckBox { name: "verbose"; label: qsTr("Verbose output") }
    IntegerField { name: "maxIter"; label: qsTr("Max iterations"); defaultValue: 500 }
}
```
