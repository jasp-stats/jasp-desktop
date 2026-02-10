<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

ColorPalette

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Inherited Properties from
  DropDown](#inherited-properties-from-dropdown)
- [Other Inherited Properties](#other-inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# ColorPalette QML Type

A dropdown preset for selecting a color palette for plots.
[More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | <a href="qml-jasp-controls-dropdown.md" translate="no">DropDown</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-colorpalette-members.md)

## Detailed Description

Extends
<a href="qml-jasp-controls-dropdown.md" translate="no">DropDown</a>
with a predefined list of color palettes commonly used in JASP plots.
Includes colorblind-friendly, Viridis, ggplot2, and other standard
palettes. Defaults to "colorblind" and binds to the R option
"colorPalette".

## R Binding

- **R Type:** `character`
- **Default:** "colorblind"

## Inherited Properties from DropDown

- **name** (string) - R option name this control binds to. Default:
  "colorPalette".
- **label** (string) - Label displayed before the dropdown. Default:
  "Color palette".
- **values** (array) - List of palette options. Pre-populated with
  standard palettes.
- **currentValue** (var) - The value of the currently selected palette.

## Other Inherited Properties

- **enabled** (bool) - Whether the control is interactive. Default:
  true.
- **visible** (bool) - Whether the control is visible. Default: true.
- **info** (string) - Info that will be used by tooltip and to generate
  the help. Default: "".
- **toolTip** (string) - This property overwrite info property, in order
  to display a simpler tooltip text. Default: "".

## Example

``` qml
ColorPalette { }
```
