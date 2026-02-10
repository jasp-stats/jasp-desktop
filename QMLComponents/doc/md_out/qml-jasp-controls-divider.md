<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

Divider

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# Divider QML Type

A horizontal line separator with an optional centered label.
[More...](#details)

<div class="table">

|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-divider-members.md)

## Detailed Description

Renders a thin horizontal rule across the full form width. When a label
is provided, it is centered over the line with a background fill to
create a visual break between sections.

<div class="admonition note">

**Note:** Divider does not bind to R options. It is a layout-only
control.

</div>

## Properties

- **label** (string) - Text displayed centered on the divider line.
  Default: "" (no label).

## Example

``` qml
Divider { label: qsTr("Advanced Options") }
```
