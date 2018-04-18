#ifndef COLUMNSMODEL_H
#define COLUMNSMODEL_H

#include <QObject>
#include <QAbstractTableModel>


#include "common.h"
#include "dataset.h"

class ColumnsModel  : public QAbstractTableModel
{
	Q_OBJECT
public:
	enum ColumnsModelRoles {
		NameRole = Qt::UserRole + 1,
		TypeRole,
		IconSourceRole,
		ToolTipRole
	 };

	ColumnsModel(QObject *parent = NULL) : QAbstractTableModel(parent) {}
	void setDataSet(DataSet *dataSet);

	virtual int rowCount(const QModelIndex &parent = QModelIndex())				const OVERRIDE { return _dataSet == NULL ? 0 : _dataSet->columnCount();  }
	virtual int columnCount(const QModelIndex &parent = QModelIndex())			const OVERRIDE { const static int roleNamesCount = roleNames().size(); return roleNamesCount; }
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
	virtual QHash<int, QByteArray> roleNames()									const OVERRIDE;

public slots:
	void refresh() { beginResetModel(); endResetModel(); }
	void refreshColumn(Column * column);

	void datasetHeaderDataChanged(Qt::Orientation orientation, int first, int last);


private:
	DataSet *_dataSet = NULL;

};



#endif // COLUMNSMODEL_H
