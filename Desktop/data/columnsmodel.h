#ifndef COLUMNSMODEL_H
#define COLUMNSMODEL_H

#include <QTransposeProxyModel>
#include "datasettablemodel.h"
#include "common.h"

///Surprisingly the columns are laid out as rows ;-)
class ColumnsModel  : public QTransposeProxyModel
{
	Q_OBJECT
public:
	enum ColumnsModelRoles {
		NameRole = Qt::UserRole + 1,
		TypeRole,
		ColumnTypeRole,
		IconSourceRole,
		DisabledIconSourceRole,
		InactiveIconSourceRole,
		ToolTipRole,
		LabelsRole,
	 };

	enum IconType { DefaultIconType, DisabledIconType, InactiveIconType };

	static ColumnsModel* singleton()	{ return _singleton; }

	ColumnsModel(DataSetTableModel * tableModel);
	~ColumnsModel()		override { if(_singleton == this) _singleton = nullptr; }

	QVariant				data(			const QModelIndex & index, int role = Qt::DisplayRole)				const	override;
	QHash<int, QByteArray>	roleNames()																			const	override;

	int						getColumnIndex(const std::string& col)												const	{ return _tableModel->getColumnIndex(col);						}
	QString					getIconFile(columnType colType, IconType type)										const;

signals:
	void namesChanged(QMap<QString, QString> changedNames);
	void columnsChanged(QStringList changedColumns);
	void columnTypeChanged(QString colName);
	void labelChanged(QString columnName, QString originalLabel, QString newLabel);
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
