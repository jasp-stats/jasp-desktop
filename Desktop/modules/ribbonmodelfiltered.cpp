#include "ribbonmodelfiltered.h"

RibbonModelFiltered::RibbonModelFiltered(QObject * parent, RibbonModel * ribbonModel) : QSortFilterProxyModel(parent)
{
	setRibbonModel(ribbonModel);
	connect(ribbonModel, &RibbonModel::invalidateFilterModel, this, &RibbonModelFiltered::invalidateFilterModel);
}

void RibbonModelFiltered::setRibbonModel(RibbonModel * ribbonModel)
{
	if(_ribbonModel	== ribbonModel)
		return;

	_ribbonModel = ribbonModel;

	setSourceModel(_ribbonModel);

	connect(ribbonModel, &RibbonModel::highlightedModuleIndexChanged, this, &RibbonModelFiltered::highlightedModuleIndexChanged);
}

bool RibbonModelFiltered::filterAcceptsRow(int source_row, const QModelIndex &) const
{
	if(source_row < 0) return false;

	return  _ribbonModel != nullptr && _ribbonModel->ribbonButtonModelAt(size_t(source_row))->enabled();
}

int RibbonModelFiltered::filteredRowToOriginal(int filteredRow) const
{
	if(_ribbonModel == nullptr || (filteredRow < 0 && filteredRow > rowCount()))
		return -1;

	return mapToSource(index(filteredRow, 0)).row();
}

int RibbonModelFiltered::originalRowToFiltered(int originalRow) const
{
	if(_ribbonModel == nullptr || (originalRow < 0 && originalRow > _ribbonModel->rowCount()))
		return -1;

	return mapFromSource(_ribbonModel->index(originalRow, 0)).row();
}
