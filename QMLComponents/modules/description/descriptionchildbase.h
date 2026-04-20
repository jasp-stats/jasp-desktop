#ifndef DESCRIPTIONCHILDBASE_H
#define DESCRIPTIONCHILDBASE_H

#include <QQuickItem>
#include "description.h"

namespace Modules
{

///
/// This class defines some shared functionality for EntryBase
/// But because that class already becomes a superclass they might as well be merged
class DescriptionChildBase : public QQuickItem
{
	Q_OBJECT
public:
	DescriptionChildBase();

signals:
	void somethingChanged();

private slots:
	void registerDescription(QQuickItem * parent);

protected:
	Description	*	_description = nullptr;
};

}

#endif // DESCRIPTIONCHILDBASE_H
