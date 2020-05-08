#ifndef DESCRIPTIONCHILDBASE_H
#define DESCRIPTIONCHILDBASE_H

#include <QQuickItem>
#include "description.h"

namespace Modules
{

class DescriptionChildBase : public QQuickItem
{
	Q_OBJECT
public:
	DescriptionChildBase();

signals:
	void somethingChanged(DescriptionChildBase * inMe);

private slots:
	void registerDescription(QQuickItem * parent);

protected:
	Description	*	_description = nullptr;
};

}

#endif // DESCRIPTIONCHILDBASE_H
