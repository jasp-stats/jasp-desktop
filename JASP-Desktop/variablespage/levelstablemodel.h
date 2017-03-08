#ifndef LEVELSTABLEMODEL_H
#define LEVELSTABLEMODEL_H

#include <QAbstractTableModel>

#include "column.h"

#include "common.h"

class LevelsTableModel : public QAbstractTableModel
{
public:
	LevelsTableModel(QObject *parent = 0);

	void setColumn(Column *column);
	void refresh();
	void clearColumn();

	int rowCount(const QModelIndex &parent) const OVERRIDE;
	int columnCount(const QModelIndex &parent) const OVERRIDE;
	QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	QVariant headerData(int section, Qt::Orientation orientation, int role) const OVERRIDE;
    Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
    bool setData(const QModelIndex & index, const QVariant & value, int role) OVERRIDE;

	void moveUp(QModelIndexList &selection);
	void moveDown(QModelIndexList &selection);
    void reverse();

private:
	Column *_column;

	void _moveRows(QModelIndexList &selection, bool up = true);
};

#endif // LEVELSTABLEMODEL_H
