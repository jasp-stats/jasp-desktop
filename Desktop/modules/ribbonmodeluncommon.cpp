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
	
	if(!_ribbonModel)
		return false;
	
	auto * button = _ribbonModel->ribbonButtonModelAt(size_t(source_row));

	if (!button)
		return false;

	return  button->remember() && !button->separator();
}

void RibbonModelUncommon::setModuleEnabled(int filteredRow, bool checked)
{
	_ribbonModel->setModuleEnabled(mapToSource(index(filteredRow, 0)).row(), checked);
}

void RibbonModelUncommon::moveModule(int from, int to)
{
	QModelIndex	fromIndex	= index(from, 0),
				toIndex		= index(to, 0);

	if(!fromIndex.isValid() || !toIndex.isValid())
		return;

	_ribbonModel->moveModule(mapToSource(fromIndex).row(), mapToSource(toIndex).row());
}

bool RibbonModelUncommon::isModule(int filteredRow)
{
	QModelIndex filteredIndex = index(filteredRow, 0);

	if(!filteredIndex.isValid())
		return false;

	RibbonButton * button = _ribbonModel->ribbonButtonModelAt(size_t(mapToSource(filteredIndex).row()));

	return button && button->module();
}
