[JASP.Controls](jasp-controls-qmlmodule.md)

Divider


# Divider QML Type

A horizontal line separator with an optional centered label.


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-divider-members.md)

## Detailed Description

Renders a thin horizontal rule across the full form width. When a label
is provided, it is centered over the line with a background fill to
create a visual break between sections.


**Note:** Divider does not bind to R options. It is a layout-only
control.


## Properties

- **label** (string) - Text displayed centered on the divider line.
  Default: "" (no label).

## Example

``` qml
Divider { label: qsTr("Advanced Options") }
```
