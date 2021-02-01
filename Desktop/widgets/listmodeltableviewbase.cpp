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

#include "log.h"
#include <QSize>
#include <fstream>
#include "listmodeltableviewbase.h"
#include "../analysis/analysisform.h"
#include "utilities/qutils.h"
#include "tableviewbase.h"
#include "textinputbase.h"
#include "gui/preferencesmodel.h"

using namespace std;

ListModelTableViewBase::ListModelTableViewBase(TableViewBase * tableView, QString tableType)
	: ListModel(tableView), _tableView(tableView), _tableType(tableType)
{
	connect(PreferencesModel::prefs(),	&PreferencesModel::uiScaleChanged,	this,	&ListModelTableViewBase::refresh);
}

QVariant ListModelTableViewBase::data(const QModelIndex &index, int role) const
{
	if (_tableTerms.rowNames.length() == 0)
		return QVariant();

	int		column	= index.column(),
			row		= index.row();

	if (column < 0 || column >= columnCount() || row < 0 || row >= rowCount())
		return QVariant();

	switch(role)
	{
	case int(specialRoles::lines):
	{
		bool	belowMeIsActive = row < rowCount() - 1,
				up				= true,
				left			= true,
				down			= !belowMeIsActive,
				right			= column == columnCount() - 1; //always draw left line and right line only if last col

		return	(left ?		1 : 0) +
				(right ?	2 : 0) +
				(up ?		4 : 0) +
				(down ?		8 : 0);
	}		
	case int(specialRoles::itemInputType):	return getItemInputType(index);
	case Qt::DisplayRole:					return QVariant(_tableTerms.values[column][row]);
	default:								return QVariant();
	}
}


int ListModelTableViewBase::getMaximumColumnWidthInCharacters(size_t columnIndex) const
{
	int maxL = 3;
	int column = int(columnIndex);

	if (column < _tableTerms.values.size())
		for (QVariant val : _tableTerms.values[column])
			maxL = std::max(val.toString().size(), maxL);

	return maxL + 3;
}


QString ListModelTableViewBase::getMaximumRowHeaderString() const
{
	int maxL = 7;

	for (QString val : _tableTerms.rowNames)
			maxL = std::max(val.size() + 4, maxL);

	QString dummyText;
	while (maxL > dummyText.length())
		dummyText += "X";

	return dummyText;
}

void ListModelTableViewBase::addColumn(bool emitStuff)
{
	if (emitStuff)
		beginResetModel();

	size_t count = size_t(columnCount());

	if (count < _maxColumn)
	{
		_tableTerms.colNames.push_back(getDefaultColName(count));
		_tableTerms.values.push_back(QVector<QVariant>(_tableTerms.rowNames.length(), _tableView->defaultEmptyValue()));
	}

	if (emitStuff)
	{
		endResetModel();

		emit columnCountChanged();
	}
}

void ListModelTableViewBase::removeColumn(size_t col, bool emitStuff)
{
	if (emitStuff)
		beginResetModel();
	int colIndex = int(col);

	if (colIndex < columnCount())
	{
		_tableTerms.values.removeAt(colIndex);
		_tableTerms.colNames.pop_back();
	}

	if (emitStuff)
	{
		endResetModel();

		emit columnCountChanged();
	}
}

void ListModelTableViewBase::addRow(bool emitStuff)
{
	if (emitStuff)
		beginResetModel();

	if (rowCount() < int(_maxRow))
	{
		_tableTerms.rowNames.push_back(getDefaultRowName(rowCount()));

		for (QVector<QVariant> & value : _tableTerms.values)
			while (value.size() < _tableTerms.rowNames.size()) //Lets make sure the data is rectangular!
				value.push_back(_tableView->defaultEmptyValue());
	}

	if (emitStuff)
	{
		endResetModel();

		emit rowCountChanged();
	}
}

void ListModelTableViewBase::removeRow(size_t row, bool emitStuff)
{
	if (emitStuff)
		beginResetModel();

	if (row < rowCount())
	{
		for (QVector<QVariant> & value : _tableTerms.values)
			value.removeAt(int(row));
		_tableTerms.rowNames.pop_back(); //Should we remove the exact right rowName? Or I guess there just generated row for row in the base..
	}

	if (emitStuff)
	{
		endResetModel();

		emit rowCountChanged();
	}
}

void ListModelTableViewBase::reset()
{
	beginResetModel();

	if (!_keepColsOnReset)	_tableTerms.colNames.clear();
	if (!_keepRowsOnReset)	_tableTerms.rowNames.clear();

	_tableTerms.values.clear();

	if (!_keepColsOnReset)
		for (int col=0; col < _tableView->initialColumnCount(); col++)
			addColumn(false);

	int rows = std::max(_tableTerms.rowNames.length(), _tableView->initialRowCount());

	if (!_keepRowsOnReset)
		for (int row=0; row < rows; row++)
			addRow();

	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
}

void ListModelTableViewBase::itemChanged(int column, int row, QVariant value, QString type)
{
	//If you change this function, also take a look at ListModelFilteredDataEntry::itemChanged
	if (column > -1 && column < columnCount() && row > -1 && row < rowCount())
	{
		if (_tableTerms.values[column][row] != value)
		{
			_tableTerms.values[column][row] = _itemType == "integer" ? value.toInt() : _itemType == "double" ? value.toDouble() : value;

		if (type != "formula") // For formula type, wait for the formulaCheckSucceeded signal before emitting modelChanged
			emit termsChanged();

			// Here we should *actually* check if specialRoles::maxColString changes and in that case: (so that the view can recalculate stuff)
			//	emit headerDataChanged(Qt::Orientation::Horizontal, column, column);
		}
	}
}

Terms ListModelTableViewBase::termsEx(const QString &what)
{
	Terms tempTerms;

	int colNb = -1;
	if (what.isEmpty() && _tableTerms.values.length() == 1)
		colNb = 0;
	else if (!what.isEmpty())
	{
		colNb = _tableTerms.colNames.indexOf(what);
		if (colNb == -1 && what.startsWith("column"))
		{
			QString tempWhat = what;
			QString colNbStr = tempWhat.remove("column");
			bool ok = false;
			colNb = colNbStr.toInt(&ok);
			if (!ok) colNb = -1;
			if (colNb > 0) colNb--;
		}
	}

	if (colNb >= 0)
	{
		if (_tableTerms.values.length() > colNb)
		{
			const QVector<QVariant> firstCol = _tableTerms.values[colNb];
			for (const QVariant& val : firstCol)
			{
				QString value = val.toString();
				if (!value.isEmpty() && value != "...")
					tempTerms.add(val.toString());
			}
		}
		else
			addControlError(tr("Column number in source use is bigger than the number of columns of %1").arg(name()));
	}
	else
	{
		if (what.isEmpty())
			addControlError(tr("No column in TableView source %1").arg(name()));
		else
			addControlError(tr("Unknown column specified (%1) in TableView source %2").arg(what).arg(name()));
	}


	return tempTerms;
}

QVariant ListModelTableViewBase::headerData( int section, Qt::Orientation orientation, int role) const
{
	if (section < 0 || section >= (orientation == Qt::Horizontal ? _tableTerms.colNames.length() : _tableTerms.rowNames.length()))
		return QVariant();

	switch(role)
	{
	case int(specialRoles::maxColString): //A query from DataSetView for the maximumlength string to be expected! This to accomodate columnwidth
	{
		QString dummyText	= headerData(section, orientation, Qt::DisplayRole).toString() + "XXXXX";
		int colWidth		= getMaximumColumnWidthInCharacters(size_t(section));

		while (colWidth > dummyText.length())
			dummyText += "X";

		return dummyText;
	}
	case int(specialRoles::maxRowHeaderString):	return getMaximumRowHeaderString();
	case Qt::DisplayRole:						return QVariant(orientation == Qt::Horizontal ? _tableTerms.colNames[section] : _tableTerms.rowNames[section]);
	case Qt::TextAlignmentRole:					return QVariant(Qt::AlignCenter);
	default:									return QVariant();
	}
}

QHash<int, QByteArray> ListModelTableViewBase::roleNames() const
{
	static QHash<int, QByteArray> roles = ListModel::roleNames();

	static bool addRoles = true;

	if (addRoles)
	{
		roles[int(specialRoles::active)]				= QString("active").toUtf8();
		roles[int(specialRoles::lines)]					= QString("lines").toUtf8();
		roles[int(specialRoles::maxColString)]			= QString("maxColString").toUtf8();
		roles[int(specialRoles::maxRowHeaderString)]	= QString("maxRowHeaderString").toUtf8();
		roles[int(specialRoles::itemInputType)]			= QString("itemInputType").toUtf8();
		addRoles = false;
	}

	return roles;
}

Qt::ItemFlags ListModelTableViewBase::flags(const QModelIndex &index) const
{
	Qt::ItemFlags flags = Qt::ItemIsSelectable | Qt::ItemIsEnabled;

	if (isEditable(index))
		flags |= Qt::ItemIsEditable;

	return flags;
}

void ListModelTableViewBase::runRScript(const QString & script)
{
	_tableView->runRScript(script);
}

bool ListModelTableViewBase::valueOk(QVariant value)
{
	bool	ok	= true;

	if		(_itemType == "double")		value.toDouble(&ok);
	else if	(_itemType == "integer")	value.toInt(&ok);

	return ok;
}

JASPControl *ListModelTableViewBase::getRowControl(const QString &key, const QString &name) const
{
	if (_itemControls.contains(key))	return _itemControls[key][name];
	else								return nullptr;
}

bool ListModelTableViewBase::addRowControl(const QString &key, JASPControl *control)
{
	_itemControls[key][control->name()] = control;

	if (control->controlType() == JASPControl::ControlType::TextField)
	{
		TextInputBase* textInput = dynamic_cast<TextInputBase*>(control);
		if (textInput && textInput->inputType() == TextInputBase::TextInputType::FormulaType)
			connect(textInput, &TextInputBase::formulaCheckSucceeded, this, &ListModelTableViewBase::formulaCheckSucceededSlot);
	}

	return true;
}

void ListModelTableViewBase::formulaCheckSucceededSlot()
{
	_tableView->updateOption();
}


void ListModelTableViewBase::initTableTerms(const TableTerms& terms)
{
	beginResetModel();

	_tableTerms = terms;

	for (auto & col : _tableTerms.values)
		if(_tableTerms.rowNames.size() < col.size())
		{
			Log::log() << "Too many rows in a column of OptionsTable for ListModelTableViewBase! Shrinking column to fit." << std::endl;
			col.resize(_tableTerms.rowNames.size());
		}
		else
			for (int row = col.size(); row < _tableTerms.rowNames.size(); row++)
				col.push_back(1);

	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
}

QString ListModelTableViewBase::getDefaultColName(size_t index) const
{
	return listView()->property("colName").toString() + " " + QString::number(index + 1);
}
