import QtQuick

/*!
    \qmltype RoundedButton
    \inqmlmodule JASP.Controls 1.0
    \brief A RectangularButton with rounded corners.

    Extends RectangularButton by adding the JASP default border radius.
    Used as the base for MenuButton and other styled buttons.

    \note This is primarily an internal UI component. Module developers typically
    use Button instead.
*/
RectangularButton 
{
	radius:			jaspTheme.borderRadius
}
