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


#ifndef LISTMODELFILTEREDDATAENTRY_H
#define LISTMODELFILTEREDDATAENTRY_H

#include "listmodeltableviewbase.h"

class ListModelFilteredDataEntry : public ListModelTableViewBase
{
	Q_OBJECT
	Q_PROPERTY(QString	filter		READ filter		WRITE setFilter		NOTIFY filterChanged	)
	Q_PROPERTY(QString	colName		READ colName	WRITE setColName	NOTIFY colNameChanged	)
	Q_PROPERTY(QString	extraCol	READ extraCol	WRITE setExtraCol	NOTIFY extraColChanged	)

public:
	explicit ListModelFilteredDataEntry(BoundQMLTableView * parent, QString tableType);

	QVariant		data(	const QModelIndex &index, int role = Qt::DisplayRole)	const	override;
	Qt::ItemFlags	flags(	const QModelIndex &index)								const	override;
	void			rScriptDoneHandler(const QString & result)								override;
	QString			filter()														const				{ return _filter;		}
	QString			colName()														const				{ return _colName;		}
	QString			extraCol()														const				{ return _extraCol;	}
	OptionsTable *	createOption()															override;
	void			initValues(OptionsTable * bindHere)										override;
	int				getMaximumColumnWidthInCharacters(size_t columnIndex)			const	override;
	bool			isEditable(const QModelIndex& index)							const	override	{ return index.column() >= _columnCount; }
	void			itemChanged(int column, int row, QVariant value, QString type)			override;

	void			refreshModel()															override;


public slots:
	void	sourceTermsChanged(const Terms* termsAdded, const Terms* termsRemoved)			override;
	void	modelChangedSlot()																override;
	void	setFilter(QString filter);
	void	dataSetChangedHandler();
	void	setColName(QString colName);
	void	setExtraCol(QString extraCol);

signals:
	void	filterChanged(QString filter);
	void	acceptedRowsChanged();
	void	colNameChanged(QString colName);
	void	extraColChanged(QString extraCol);

private slots:
	void	runFilter(QString filter);

private:
	void	setAcceptedRows(std::vector<bool> newRows);
	void	setAcceptedRowsTrue()		{ setAcceptedRows(std::vector<bool>(getDataSetRowCount(), true)); }
	size_t	getDataSetRowCount();
	void	fillTable();

	QString						_filter,
								_colName = "data",
								_extraCol;
	std::vector<bool>			_acceptedRows;
	std::vector<size_t>			_filteredRowToData;
	std::map<size_t, double>	_enteredValues;
	int							_editableColumn = 0;
	std::vector<std::string>	_dataColumns,
								_extraColsStr;
};

#endif // LISTMODELFILTEREDDATAENTRY_H
