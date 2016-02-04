#ifndef VARIABLESTABLEMODEL_H
#define VARIABLESTABLEMODEL_H

#include <QAbstractTableModel>

#include "dataset.h"

#include "common.h"

class VariablesTableModel : public QAbstractTableModel
{
public:
	VariablesTableModel(QObject *parent = NULL);

	void setDataSet(DataSet *dataSet);
	void clearDataSet();

	QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	int rowCount(const QModelIndex &parent) const OVERRIDE;
	int columnCount(const QModelIndex &parent) const OVERRIDE;


private:
	DataSet *_dataSet;

};

#endif // VARIABLESTABLEMODEL_H
