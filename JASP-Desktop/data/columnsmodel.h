#ifndef COLUMNSMODEL_H
#define COLUMNSMODEL_H

#include <QAbstractTableModel>
#include "datasettablemodel.h"
#include "common.h"

///Surprisingly the columns are laid out as rows ;-)
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

	ColumnsModel(DataSetTableModel * tableModel) : QAbstractTableModel(tableModel), _tableModel(tableModel)
	{
		connect(_tableModel, &DataSetTableModel::headerDataChanged,		this, &ColumnsModel::onHeaderDataChanged);
		connect(_tableModel, &DataSetTableModel::dataChanged,			this, &ColumnsModel::onDataChanged		);
		connect(_tableModel, &DataSetTableModel::modelAboutToBeReset,	this, &ColumnsModel::beginResetModel	);
		connect(_tableModel, &DataSetTableModel::modelReset,			this, &ColumnsModel::endResetModel		);
	}

	QVariant				data(			const QModelIndex & index, int role = Qt::DisplayRole)				const	override;
	QHash<int, QByteArray>	roleNames()																			const	override;
	int						rowCount(	const QModelIndex &parent = QModelIndex())								const	override;
	int						columnCount(const QModelIndex &parent = QModelIndex())								const	override;
	QVariant				headerData(	int section, Qt::Orientation orientation, int role = Qt::DisplayRole )	const	override;

private slots:
	void onHeaderDataChanged(Qt::Orientation orientation, int first, int last);
	void onDataChanged(const QModelIndex &topLeft, const QModelIndex &bottomRight, const QVector<int> &roles);


private:
	DataSetTableModel * _tableModel = nullptr;
};



#endif // COLUMNSMODEL_H
