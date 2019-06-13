//
// Copyright (C) 2013-2019 University of Amsterdam
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

#ifndef LISTMODELTABLEVIEWBASE_H
#define LISTMODELTABLEVIEWBASE_H

#include "listmodel.h"
#include "common.h"
#include "data/datasetpackage.h"
#include "analysis/options/optionstable.h"

class BoundQMLTableView;

///This class makes sure the roles that DataSetView expects are implemented and returned.
class ListModelTableViewBase : public ListModel
{
	Q_OBJECT
	Q_PROPERTY(int columnCount	READ columnCount	NOTIFY columnCountChanged)
	Q_PROPERTY(int rowCount		READ rowCount		NOTIFY rowCountChanged)

public:
	enum class	specialRoles		{ active = Qt::UserRole, lines, maxColString };

	explicit						ListModelTableViewBase(BoundQMLTableView * tableView, QString tableType);

	QHash<int, QByteArray>			roleNames() const override;

				int					rowCount(	const QModelIndex & = QModelIndex())									const	override {	return _rowNames.length();				}
				int					columnCount(const QModelIndex & = QModelIndex())									const	override {	return static_cast<int>(_columnCount);	}
				QVariant			data(		const QModelIndex &index, int role = Qt::DisplayRole)					const	override;
				Qt::ItemFlags		flags(		const QModelIndex &index)												const	override;
				QVariant			headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole )	const	override;

	virtual		int					getMaximumColumnWidthInCharacters(size_t columnIndex)								const;

				void				addColumn();
				void				removeColumn(size_t index);
				void				reset();
	virtual		void				itemChanged(int column, int row, double value);
				void				refreshModel() { return ListModel::refresh(); }
	virtual		void				initValues(OptionsTable * bindHere)		{}
	virtual		QString				getColName(size_t index)				{ return "Col " + QString::fromStdString(std::to_string(index)); }
	virtual		OptionsTable *		createOption()							{ return new OptionsTable(); }
	virtual		void				modelChangedSlot()						{}

				const QVector<QVector<double> >	&	values()	const { return _values;		}
				const QVector<QString>			&	rowNames()	const { return _rowNames;	}
				const QVector<QString>			&	colNames()	const { return _colNames;	}

				void runRScript(		const QString & script);
	virtual		void rScriptDoneHandler(const QString & result) { throw std::runtime_error("runRScript done but handler not implemented!\nImplement an override for RScriptDoneHandler and usesRScript\nResult was: "+result.toStdString()); }

signals:
	void columnCountChanged();
	void rowCountChanged();
	void itemChangedSignal(int column, int row, double value);

protected:

	BoundQMLTableView		*	_tableView		= nullptr;
	OptionsTable			*	_boundTo		= nullptr;

	const size_t				_maxColumn		= 10;
	QVector<QString>			_rowNames,
								_colNames;
	QVector<QVector<double> >	_values;
	int							_rowSelected	= -1;
	QString						_tableType;
	size_t						_columnCount	= 0;
};

#endif // LISTMODELTABLEVIEWBASE_H
