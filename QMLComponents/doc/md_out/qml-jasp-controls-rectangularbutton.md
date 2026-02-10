<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

RectangularButton

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Signals](#signals)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# RectangularButton QML Type

A styled rectangular button with optional icon and text.
[More...](#details)

<div class="table">

|                   |                                                |
|-------------------|------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                     |
| Inherited By:     | <a href="qml-jasp-controls-roundedbutton.md" 
                     translate="no">RoundedButton</a>                |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-rectangularbutton-members.md)

## Detailed Description

A Rectangle-based button supporting text, icon, or both. Provides hover,
pressed, and disabled states with JASP theming. Used as the base for
<a href="qml-jasp-controls-roundedbutton.md"
translate="no">RoundedButton</a> and
<a href="qml-jasp-controls-menubutton.md"
translate="no">MenuButton</a>.

<div class="admonition note">

**Note:** This is primarily an internal UI component. Module developers
typically use Button instead.

</div>

## Properties

- **text** (string) - Button label text. Default: "".
- **toolTip** (string) - Tooltip shown on hover. Default: "".
- **iconSource** (string) - Path to the button icon. Default: "".
- **showIconAndText** (bool) - Show both icon and text simultaneously.
  Default: false.
- **centerText** (bool) - Center the text within the button. Default:
  true.
- **iconLeft** (bool) - Place icon on the left side. Default: true.
- **isLink** (bool) - Style as a hyperlink. Default: false.

## Signals

- **clicked()** - Emitted when the button is clicked.

## Example

``` qml
RectangularButton {
    text: qsTr("Apply")
    iconSource: jaspTheme.iconPath + "confirm.png"
}
```
