[JASP.Controls](jasp-controls-qmlmodule.md)

FileSelector


# FileSelector QML Type

A text field with a browse button for selecting files or directories.


|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | [TextField](qml-jasp-controls-textfield.md) |


- [List of all members, including inherited
  members](qml-jasp-controls-fileselector-members.md)

## Detailed Description

Extends
[TextField](qml-jasp-controls-textfield.md)
with a "Browse" button that opens a native file dialog. Supports saving,
loading, directory selection, and file type filtering.

## R Binding

- **R Type:** `character` (file path)
- **Default:** ""

## Properties

- **save** (bool) - If true, opens a save dialog; otherwise a load
  dialog. Default: true.
- **caption** (string) - Caption for the file dialog. Default:
  auto-generated from save.
- **filter** (string) - File filter pattern (e.g. "\*.csv"). Default:
  "\*".
- **buttonText** (string) - Text on the browse button. Default:
  "Browse".
- **directory** (bool) - If true, browses for a directory instead of a
  file. Default: false.
- **multiple** (bool) - Allow selecting multiple files (load mode only).
  Default: false.

## Inherited Properties from TextField

- **name** (string) - R option name this control binds to. Default: "".
- **value** (string) - Current file path. Default: "".
- **label** (string) - Label displayed before the field. Default:
  auto-generated from save.

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
FileSelector {
    name: "outputFile"
    save: true
    filter: "*.csv"
}
```
