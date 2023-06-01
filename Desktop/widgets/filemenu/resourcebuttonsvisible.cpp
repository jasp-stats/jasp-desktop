#include "resourcebuttonsvisible.h"

ResourceButtonsVisible::ResourceButtonsVisible(QObject * parent, ResourceButtons * ResourceButtons) : QSortFilterProxyModel(parent)
{
	setResourceButtons(ResourceButtons);
}

void ResourceButtonsVisible::setResourceButtons(ResourceButtons * ResourceButtons)
{
	if(_resourceButtons	== ResourceButtons)
		return;

	_resourceButtons = ResourceButtons;

	setSourceModel(_resourceButtons);
}

bool ResourceButtonsVisible::filterAcceptsRow(int source_row, const QModelIndex &) const
{
	if(source_row < 0) return false;

	return  _resourceButtons != nullptr && _resourceButtons->data(_resourceButtons->index(source_row, 0), ResourceButtons::VisibleRole).toBool();
}

int ResourceButtonsVisible::filteredRowToOriginal(int filteredRow) const
{
	if(_resourceButtons == nullptr || (filteredRow < 0 && filteredRow > rowCount()))
		return -1;

	return mapToSource(index(filteredRow, 0)).row();
}

int ResourceButtonsVisible::originalRowToFiltered(int originalRow) const
{
	if(_resourceButtons == nullptr || (originalRow < 0 && originalRow > _resourceButtons->rowCount()))
		return -1;

	return mapFromSource(_resourceButtons->index(originalRow, 0)).row();
}
