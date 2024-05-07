#include "groupboxbase.h"

GroupBoxBase::GroupBoxBase(QQuickItem* parent) 
	: JASPControl(parent) 
{ 
	_controlType = JASPControl::ControlType::GroupBox; 
}

bool GroupBoxBase::infoLabelIsHeader() const
{ 
	return true; 
}
