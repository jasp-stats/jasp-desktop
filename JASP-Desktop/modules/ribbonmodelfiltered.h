#ifndef RIBBONMODELFILTERED_H
#define RIBBONMODELFILTERED_H

#include <QSortFilterProxyModel>
#include "ribbonmodel.h"

class RibbonModelFiltered : public QSortFilterProxyModel
{
public:
	RibbonModelFiltered(QObject * parent = nullptr, RibbonModel * ribbonModel = nullptr);

	void setRibbonModel(RibbonModel * ribbonModel);

	bool filterAcceptsRow(int source_row, const QModelIndex &source_parent) const override;

	Q_INVOKABLE int filteredRowToOriginal(int filteredRow);
	Q_INVOKABLE int originalRowToFiltered(int originalRow);

private:
	RibbonModel * _ribbonModel = nullptr;
};

#endif // RIBBONMODELFILTERED_H
