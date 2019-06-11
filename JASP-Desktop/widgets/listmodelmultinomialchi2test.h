//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#ifndef LISTMODELMULTINOMIALCHI2TEST_H
#define LISTMODELMULTINOMIALCHI2TEST_H

#include "listmodel.h"
#include "common.h"
#include "data/datasetpackage.h"


class ListModelMultinomialChi2Test : public ListModel
{
	Q_OBJECT

public:
	enum class	specialRoles { active = Qt::UserRole, lines, maxColString };

	explicit						ListModelMultinomialChi2Test(QMLListView* parent, QString tableType);

	QHash<int, QByteArray>			roleNames() const override;

				int					rowCount(const QModelIndex &parent = QModelIndex())									const	override;
				int					columnCount(const QModelIndex &parent = QModelIndex())								const	override;
				QVariant			data(const QModelIndex &index, int role = Qt::DisplayRole)							const	override;
				QVariant			headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole )	const	override;
				Qt::ItemFlags		flags(const QModelIndex &index)														const	override;

				int					getMaximumColumnWidthInCharacters(size_t columnIndex) const;

				void				addColumn();
				void				removeColumn(size_t index);
				void				reset();
				void				itemChanged(int column, int row, double value);
				const QVector<QVector<double> >& values() const { return _values; }
				const QVector<QString>&	rowNames() const { return _rowNames; }
				const QVector<QString>& colNames() const { return _colNames; }
				void				initValues(const std::vector<std::string>& colNames, std::vector<std::string>& levels, const std::vector<std::vector<double> >& values);

public slots:
	void sourceTermsChanged(Terms* termsAdded, Terms* termsRemoved) override;

	void refreshModel() { return ListModel::refresh(); }


private:
	size_t						_columnCount = 0;
	QVector<QString>			_rowNames;
	QVector<QString>			_colNames;
	QVector<QVector<double> >	_values;
	int							_rowSelected = -1;
	QString						_tableType;

	QString	_getColName(size_t index);

	const size_t	_maxColumn = 10;
};

#endif // LISTMODELMULTINOMIALCHI2TEST_H

