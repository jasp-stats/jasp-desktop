#ifndef COLUMNSMODEL_H
#define COLUMNSMODEL_H

#include <QTransposeProxyModel>
#include "datasettablemodel.h"
#include "common.h"
#include "analysis/variableinfo.h"

/// 
/// Model used by the filter-drag-n-drop to give all the columns and their datatypes
/// The columns are layed out as rows to facilitate that
class ColumnsModel  : public QTransposeProxyModel, public VariableInfoProvider
{
	Q_OBJECT
public:
	enum ColumnsModelRoles {
		NameRole = Qt::UserRole + 1,
		TypeRole,
		ColumnTypeRole,
		IconSourceRole,
		ToolTipRole
	 };

	static ColumnsModel* singleton()	{ return _singleton; }

	ColumnsModel(DataSetTableModel * tableModel);
	~ColumnsModel()		override { if(_singleton == this) _singleton = nullptr; }

	QVariant					data(			const QModelIndex & index, int role = Qt::DisplayRole)				const	override;
	QHash<int, QByteArray>		roleNames()																			const	override;
	int							columnCount(const QModelIndex & = QModelIndex())									const	override	{ return 1;	}
	int							getColumnIndex(const std::string& col)												const				{ return _tableModel->getColumnIndex(col);	}

	QVariant					provideInfo(VariableInfo::InfoType info, const QString& colName = "", int row = 0)	const	override;
	QAbstractItemModel*			providerModel()																				override	{ return this;	}

signals:
	void namesChanged(QMap<QString, QString> changedNames);
	void columnsChanged(QStringList changedColumns);
	void columnTypeChanged(QString colName);
	void labelsChanged(QString columnName, QMap<QString, QString> changedLabels);
	void labelsReordered(QString columnName);

public slots:
	void datasetChanged(	QStringList				changedColumns,
							QStringList				missingColumns,
							QMap<QString, QString>	changeNameColumns,
							bool					rowCountChanged,
							bool					hasNewColumns);

private:
	void refresh();

	DataSetTableModel * _tableModel = nullptr;

	static ColumnsModel* _singleton;
};



#endif // COLUMNSMODEL_H
