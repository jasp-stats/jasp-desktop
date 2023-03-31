#include "ribbonmodeluncommon.h"

RibbonModelUncommon::RibbonModelUncommon(QObject * parent, RibbonModel * ribbonModel) : QSortFilterProxyModel(parent)
{
	setRibbonModel(ribbonModel);
}

void RibbonModelUncommon::setRibbonModel(RibbonModel * ribbonModel)
{
	if(_ribbonModel	== ribbonModel)
		return;

	_ribbonModel = ribbonModel;

	setSourceModel(_ribbonModel);
}

bool RibbonModelUncommon::filterAcceptsRow(int source_row, const QModelIndex &) const
{
	if(source_row < 0) return false;

	return  _ribbonModel != nullptr && (!_ribbonModel->ribbonButtonModelAt(size_t(source_row))->isCommon() || !_ribbonModel->ribbonButtonModelAt(size_t(source_row))->isBundled());
}

void RibbonModelUncommon::setModuleEnabled(int filteredRow, bool checked)
{
	_ribbonModel->setModuleEnabled(mapToSource(index(filteredRow, 0)).row(), checked);
}
