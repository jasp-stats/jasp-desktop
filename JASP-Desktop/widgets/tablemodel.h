#ifndef TABLEMODEL_H
#define TABLEMODEL_H

#include <QAbstractTableModel>

class TableModel : public QAbstractTableModel
{
	Q_OBJECT
public:
	TableModel(QObject *parent) : QAbstractTableModel(parent) { }
	virtual void mimeDataMoved(const QModelIndexList &indexes) { Q_UNUSED(indexes); }

};

#endif // TABLEMODEL_H
