#ifndef LEVELSTABLEMODEL_H
#define LEVELSTABLEMODEL_H

#include <QAbstractTableModel>

#include "column.h"

#include "common.h"
#include "dataset.h"

class LevelsTableModel : public QAbstractTableModel
{
	Q_OBJECT
	Q_PROPERTY(int filteredOut READ filteredOut NOTIFY filteredOutChanged)
public:
	LevelsTableModel(QObject *parent = 0);

	enum class Roles {
		ValueRole = Qt::UserRole + 1,
		LabelRole,
		FilterRole
	};
	Q_ENUM(Roles)

	void setColumn(Column *column);
	void clearColumn();

	int rowCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	int columnCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	QVariant headerData(int section, Qt::Orientation orientation, int role) const OVERRIDE;
    Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
    bool setData(const QModelIndex & index, const QVariant & value, int role) OVERRIDE;

	void moveUp(QModelIndexList &selection);
	void moveDown(QModelIndexList &selection);

	Q_INVOKABLE void reverse();
	Q_INVOKABLE void setColumnFromQML(int columnIndex) { setColumn(&_dataSet->column(columnIndex)); }
	Q_INVOKABLE void moveUpFromQML(QVariantList selection) { QModelIndexList List = convertQVariantList_to_QModelIndexList(selection); moveUp(List); }
	Q_INVOKABLE void moveDownFromQML(QVariantList selection) { QModelIndexList List = convertQVariantList_to_QModelIndexList(selection); moveDown(List); }
	QModelIndexList convertQVariantList_to_QModelIndexList(QVariantList selection);

	virtual QHash<int, QByteArray> roleNames() const OVERRIDE;

	Q_INVOKABLE bool setAllowFilterOnLabel(int row, bool newAllowValue);
	Q_INVOKABLE bool allowFilter(int row);
	Q_INVOKABLE void resetFilterAllows();

	void setDataSet(DataSet * thisDataSet);
	int filteredOut();

public slots:
	void refresh();
	void refreshColumn(Column * column);
	void refreshConnectedModelsToName(Column * column) { emit refreshConnectedModelsByName(column->name());	}


signals:
	void refreshConnectedModels(Column * column);
	void refreshConnectedModelsByName(std::string columnName);
	void resizeLabelColumn();
	void labelFilterChanged();
	void notifyColumnHasFilterChanged(int column); //should be on column but column is not a Qt object.
	void filteredOutChanged();

private:
	Column		*_column;
	DataSet		*_dataSet = NULL;
	std::string _colName = "";

	void _moveRows(QModelIndexList &selection, bool up = true);
	int currentColumnIndex();

};

#endif // LEVELSTABLEMODEL_H
