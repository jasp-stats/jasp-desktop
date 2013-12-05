#ifndef DATASETTABLEMODEL_H
#define DATASETTABLEMODEL_H

#include <QModelIndex>
#include <QAbstractTableModel>
#include <QIcon>

#include "common.h"
#include "dataset.h"

class DataSetTableModel : public QAbstractTableModel
{
    Q_OBJECT

public:
    explicit DataSetTableModel(QObject *parent = 0);

    void setDataSet(DataSet *dataSet);
	void clearDataSet();

    virtual int rowCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
    virtual int columnCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
    virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
    virtual QVariant headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole ) const OVERRIDE;
    virtual bool setData(const QModelIndex &index, const QVariant &value, int role) OVERRIDE;
    virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;

	void setColumnType(int columnIndex, Column::ColumnType newColumnType);

    
signals:

	void badDataEntered(const QModelIndex index);
    
private:
	DataSet *_dataSet;

	QIcon _nominalIcon;
	QIcon _ordinalIcon;
	QIcon _scaleIcon;
};

#endif // DATASETTABLEMODEL_H
