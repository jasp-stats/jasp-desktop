#ifndef COLUMNSMODEL_H
#define COLUMNSMODEL_H

#include <QAbstractTableModel>
#include "datasettablemodel.h"
#include "variableinfo.h"

/// 
/// Model used by the filter-drag-n-drop to give all the columns and their datatypes
/// The columns are layed out as rows to facilitate that
class ColumnsModel  : public QAbstractTableModel, public VariableInfoProvider
{
	Q_OBJECT
public:
	enum ColumnsModelRoles {
		NameRole = Qt::UserRole + 1,
		TypeRole,
		ColumnTypeRole,
		ComputedColumnTypeRole,
		IconSourceRole,
		ToolTipRole
	 };
											ColumnsModel(DataSetTableModel * tableModel);
											~ColumnsModel()		override;

				QVariant					data(			const QModelIndex & index, int role = Qt::DisplayRole)				const	override;
				int							rowCount(		const QModelIndex &parent = QModelIndex())							const	override;
				QHash<int, QByteArray>		roleNames()																			const	override;
				int							columnCount(	const QModelIndex &parent = QModelIndex())							const	override;
				QStringList					getColumnNames()																	const;
	Q_INVOKABLE	int							getColumnType(const QString & name)													const;
	Q_INVOKABLE	QString						getColumnIcon(int columnType)														const;
	Q_INVOKABLE	QString						getColumnIcon(int columnType, bool isTransformed)									const;
				QString						getColumnIcon(columnType colType)													const;
				int							getColumnIndex(const std::string & col)												const				{ return _tableModel->getColumnIndex(col);	}
	Q_INVOKABLE QString						getColumnDescription(const QString & name)											const;
	Q_INVOKABLE	QString						getColumnIconTransform(int columnType)												const;
				QString						getColumnIconTransform(columnType colType)											const;
				QString						getColumnTransformedToolTip(const QString & name, columnType transformedTo)			const;
	Q_INVOKABLE	QString						getColumnTransformedToolTip(const QString & name, int transformedTo)				const;

				QVariant					provideInfo(varInfoType info, const QString& colName = "", int row = 0)		const	override;
	Q_INVOKABLE	QVariant					provideInfoAt(varInfoType info, int colIndex, int row = 0)					const;
				bool						absorbInfo(	varInfoType info, const QString& name, int row, QVariant value)			override;
				QAbstractItemModel		*	providerModel()																					override	{ return this;	}

	static		ColumnsModel			*	singleton()	{ return _singleton; }

public slots:
	void datasetChanged(int dataSetId, QStringList changedColumns, QStringList missingColumns, QMap<QString, QString> changeNameColumns, bool rowCountChanged, bool hasNewColumns);
	void refresh() { beginResetModel(); endResetModel(); }

signals:
	void columnNamesChanged(QMap<QString, QString>	changedNames);
	void columnsChanged(	QStringList				changedColumns);
	void columnTypeChanged(	QString					colName);
	void labelsChanged(		QString					columnName, QMap<QString, QString> changedLabels);
	void labelsReordered(	QString					columnName);
	void filterChanged();
	void dataSetChanged();

private:
	DataSetTableModel		* _tableModel	= nullptr;
	static ColumnsModel		* _singleton;
};



#endif // COLUMNSMODEL_H
