#ifndef RibbonModelUncommon_H
#define RibbonModelUncommon_H

#include <QSortFilterProxyModel>
#include "ribbonmodel.h"

class RibbonModelUncommon : public QSortFilterProxyModel
{
	Q_OBJECT

public:
	RibbonModelUncommon(QObject * parent = nullptr, RibbonModel * ribbonModel = nullptr);

	void setRibbonModel(RibbonModel * ribbonModel);

	bool filterAcceptsRow(int source_row, const QModelIndex &source_parent) const override;

	Q_INVOKABLE void setModuleEnabled(int filteredRow, bool checked);

private:
	RibbonModel		*_ribbonModel			= nullptr;
};

#endif // RibbonModelUncommon_H
