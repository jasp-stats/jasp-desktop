//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//


#ifndef FACTORSMODEL_H
#define FACTORSMODEL_H

#include <QObject>
#include <QAbstractTableModel>
#include <QStringList>


#include "common.h"
#include "dataset.h"
// #include "labels.h"

class FactorsModel  : public QAbstractTableModel
{
	Q_OBJECT
public:
	enum FactorsModelRoles {
		NameRole = Qt::UserRole + 1,
		TypeRole
	 };

	FactorsModel(QObject *parent = NULL) : QAbstractTableModel(parent) {}
	void setDataSet(DataSet *dataSet);
	// void setFactors(Labels &labels);
	void setFactors(QStringList);

	// int rowCount(const QModelIndex &parent = QModelIndex())				const override { return _dataSet == NULL ? 0 : _dataSet->columnCount();  }
	// int columnCount(const QModelIndex &parent = QModelIndex())			const override { const static int roleNamesCount = roleNames().size(); return roleNamesCount; }

	int rowCount(const QModelIndex &parent = QModelIndex())				const override { return _labels.length();  }
	int columnCount(const QModelIndex &parent = QModelIndex())			const override { const static int roleNamesCount = roleNames().size(); return roleNamesCount; }


	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;
	QHash<int, QByteArray> roleNames()									const override;

public slots:
	void refresh() { beginResetModel(); endResetModel(); }
	void refreshColumn(Column * column);

	void datasetHeaderDataChanged(Qt::Orientation orientation, int first, int last);

private:
	DataSet *_dataSet = NULL;
	QStringList _labels = QStringList();

};


#endif // FACTORSMODEL_H
