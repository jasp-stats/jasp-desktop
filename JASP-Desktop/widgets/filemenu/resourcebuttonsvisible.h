#ifndef ResourceButtonsVisible_H
#define ResourceButtonsVisible_H

#include <QSortFilterProxyModel>
#include "resourcebuttons.h"


class ResourceButtonsVisible : public QSortFilterProxyModel
{
	Q_OBJECT

public:
	ResourceButtonsVisible(QObject * parent = nullptr, ResourceButtons * resourceButtons = nullptr);

	void setResourceButtons(ResourceButtons * resourceButtons);

	bool filterAcceptsRow(int source_row, const QModelIndex &source_parent) const override;

	Q_INVOKABLE int filteredRowToOriginal(int filteredRow) const;
	Q_INVOKABLE int originalRowToFiltered(int originalRow) const;

private:
	ResourceButtons		*_resourceButtons = nullptr;
};

#endif // ResourceButtonsVisible_H
