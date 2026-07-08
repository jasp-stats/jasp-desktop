#include "colorpickerbase.h"

ColorPickerBase::ColorPickerBase(QQuickItem* parent)
	: JASPControl(parent), BoundControlBase(this)
{
	_controlType = ControlType::ColorPicker;
}

bool ColorPickerBase::isJsonValid(const Json::Value &value) const
{
	return value.type() == Json::stringValue;
}

Json::Value ColorPickerBase::createJson() const
{
	return fq(value());
}

QString ColorPickerBase::value() const
{
	return property("value").toString();
}
