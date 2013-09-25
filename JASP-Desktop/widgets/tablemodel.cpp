#include "tablemodel.h"

#include <QTimer>

TableModel::TableModel(QObject *parent) :
	QAbstractTableModel(parent)
{
	_rowRemovalScheduled = false;
}

bool TableModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (index.isValid() == false)
		return false;

	if (role == Qt::DisplayRole && value.isNull())
	{
		int row = index.row();

		if ( ! _rowsToRemove.contains(row))
			_rowsToRemove.append(row);

		if ( ! _rowRemovalScheduled)
		{
			_rowRemovalScheduled = true;
			QTimer::singleShot(0, this, SLOT(removeEmptyRows()));
		}

		return true;
	}

	return false;
}

void TableModel::removeEmptyRows()
{
	_rowRemovalScheduled = false;

	qSort(_rowsToRemove);

	if (_rowsToRemove.isEmpty())
		return;

	QListIterator<int> it = QListIterator<int>(_rowsToRemove);
	it.toBack();

	while (it.hasPrevious())
	{
		int row = it.previous();
		this->removeRow(row);
	}

	_rowsToRemove.clear();
}
