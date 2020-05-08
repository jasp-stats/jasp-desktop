#include "descriptionchildbase.h"

namespace Modules
{

DescriptionChildBase::DescriptionChildBase()
{
	connect(this, &QQuickItem::parentChanged, this, &DescriptionChildBase::registerDescription);
}


void DescriptionChildBase::registerDescription(QQuickItem * parent)
{
	Description * newDesc = dynamic_cast<Description*>(parent);

	if(newDesc != _description)
	{
		if(_description)
			_description->removeChild(this);

		_description = newDesc;

		if(_description)
			_description->addChild(this);
	}
}

}
