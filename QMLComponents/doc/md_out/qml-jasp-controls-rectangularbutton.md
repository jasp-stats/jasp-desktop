[JASP.Controls](jasp-controls-qmlmodule.md)

RectangularButton


### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Signals](#signals)
- [Example](#example)


# RectangularButton QML Type

A styled rectangular button with optional icon and text.


|                   |                                                |
|-------------------|------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                     |
| Inherited By:     | [RoundedButton](qml-jasp-controls-roundedbutton.md)                |


- [List of all members, including inherited
  members](qml-jasp-controls-rectangularbutton-members.md)

## Detailed Description

A Rectangle-based button supporting text, icon, or both. Provides hover,
pressed, and disabled states with JASP theming. Used as the base for
[RoundedButton](qml-jasp-controls-roundedbutton.md) and
[MenuButton](qml-jasp-controls-menubutton.md).


**Note:** This is primarily an internal UI component. Module developers
typically use Button instead.


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
