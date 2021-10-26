#ifndef RIBBONMODELFILTERED_H
#define RIBBONMODELFILTERED_H

#include <QSortFilterProxyModel>
#include "ribbonmodel.h"

///
/// Filters the RibbonButtons delivered by RibbonModel to only show those that are active or "enabled" 
/// This model is used by the actual ribbon in the GUI
class RibbonModelFiltered : public QSortFilterProxyModel
{
	Q_OBJECT
	Q_PROPERTY(int highlightedModuleIndex READ highlightedModuleIndex NOTIFY highlightedModuleIndexChanged)

public:
	RibbonModelFiltered(QObject * parent = nullptr, RibbonModel * ribbonModel = nullptr);

	void setRibbonModel(RibbonModel * ribbonModel);

	bool filterAcceptsRow(int source_row, const QModelIndex &source_parent) const override;

	Q_INVOKABLE int filteredRowToOriginal(int filteredRow) const;
	Q_INVOKABLE int originalRowToFiltered(int originalRow) const;
/// 
	int highlightedModuleIndex() const { return _ribbonModel == nullptr || _ribbonModel->highlightedModuleIndex() == -1 ? -1 : originalRowToFiltered(_ribbonModel->highlightedModuleIndex()); }

signals:
	void highlightedModuleIndexChanged(int highlightedModuleIndex);

private:
	RibbonModel		*_ribbonModel			= nullptr;
	int				_highlightedModuleIndex	= -1;
};

#endif // RIBBONMODELFILTERED_H
