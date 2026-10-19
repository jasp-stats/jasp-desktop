#ifndef COLORPICKERBASE_H
#define COLORPICKERBASE_H

#include "jaspcontrol.h"
#include "boundcontrols/boundcontrolbase.h"


class ColorPickerBase : public JASPControl, public BoundControlBase
{
	Q_OBJECT
	QML_ELEMENT

public:
	ColorPickerBase(QQuickItem* parent = nullptr);

	bool		isJsonValid(const Json::Value& value)		const	override;
	Json::Value createJson()								const	override;
	QString		value()										const;
};

#endif // COLORPICKERBASE_H
