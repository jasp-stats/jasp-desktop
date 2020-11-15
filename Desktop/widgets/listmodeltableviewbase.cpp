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
#include "analysis/options/optionstring.h"
#include "analysis/options/optiondoublearray.h"
#include "gui/preferencesmodel.h"

using namespace std;

ListModelTableViewBase::ListModelTableViewBase(TableViewBase * tableView, QString tableType)
	: ListModel(tableView), _tableView(tableView), _tableType(tableType)
{
	connect(this, &ListModel::modelChanged, this, &ListModelTableViewBase::modelChangedSlot);
	connect(PreferencesModel::prefs(),	&PreferencesModel::uiScaleChanged,	this,	&ListModelTableViewBase::refresh);
}

QVariant ListModelTableViewBase::data(const QModelIndex &index, int role) const
{
	if (_rowNames.length() == 0)
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
	case Qt::DisplayRole:					return QVariant(_values[column][row]);
	default:								return QVariant();
	}
}


int ListModelTableViewBase::getMaximumColumnWidthInCharacters(size_t columnIndex) const
{
	int maxL = 3;

	if(columnIndex < _values.size())
		for(QVariant val : _values[columnIndex])
			maxL = std::max(val.toString().size(), maxL);

	return maxL + 3;
}


QString ListModelTableViewBase::getMaximumRowHeaderString() const
{
	int maxL = 7;

	for(QString val : _rowNames)
			maxL = std::max(val.size() + 4, maxL);

	QString dummyText;
	while(maxL > dummyText.length())
		dummyText += "X";

	return dummyText;
}

void ListModelTableViewBase::addColumn(bool emitStuff)
{
	if(emitStuff)
		beginResetModel();

	if (columnCount() < _maxColumn)
	{
		_colNames.push_back(getDefaultColName(columnCount()));
		_values.push_back(QVector<QVariant>(_rowNames.length(), _defaultCellVal));
		_columnCount++;
	}

	if(emitStuff)
	{
		endResetModel();

		emit columnCountChanged();
		emit modelChanged();
	}
}

void ListModelTableViewBase::removeColumn(size_t col, bool emitStuff)
{
	if(emitStuff)
		beginResetModel();

	if (col < columnCount())
	{
		_values.removeAt(int(col));
		_colNames.pop_back();	
		_columnCount--;
	}

	if(emitStuff)
	{
		endResetModel();

		emit columnCountChanged();
		emit modelChanged();
	}
}

void ListModelTableViewBase::addRow(bool emitStuff)
{
	if(emitStuff)
		beginResetModel();

	if (rowCount() < _maxRow)
	{
		_rowNames.push_back(getDefaultRowName(rowCount()));
		_rowCount++;

		for (QVector<QVariant> & value : _values)
			while(value.size() < _rowCount) //Lets make sure the data is rectangular!
				value.push_back(_defaultCellVal);
	}

	if(emitStuff)
	{
		endResetModel();

		emit rowCountChanged();
		emit modelChanged();
	}
}

void ListModelTableViewBase::removeRow(size_t row, bool emitStuff)
{
	if(emitStuff)
		beginResetModel();

	if (row < rowCount())
	{
		for (QVector<QVariant> & value : _values)
			value.removeAt(int(row));
		_rowNames.pop_back(); //Should we remove the exact right rowName? Or I guess there just generated row for row in the base..
		_rowCount--;
	}

	if(emitStuff)
	{
		endResetModel();

		emit rowCountChanged();
		emit modelChanged();
	}
}

void ListModelTableViewBase::reset()
{
	beginResetModel();

	if(!_keepColsOnReset)
	{
		_colNames.clear();
		_columnCount	= 0;
	}

	if(!_keepRowsOnReset)
	{
		_rowNames.clear();
		_rowCount		= 0;
	}

	_values.clear();

	if(!_keepColsOnReset)
		for(size_t col=0; col < _initialColCnt; col++)
			addColumn(false);

	size_t rows = std::max(size_t(_rowNames.length()), _initialRowCnt);

	if(!_keepRowsOnReset)
		for(size_t row=0; row < rows; row++)
			addRow();

	emit columnCountChanged();
	emit rowCountChanged();
	emit modelChanged();

	endResetModel();
}

void ListModelTableViewBase::itemChanged(int column, int row, QVariant value, QString type)
{
	//If you change this function, also take a look at ListModelFilteredDataEntry::itemChanged
	if (column > -1 && column < columnCount() && row > -1 && row < _rowNames.length())
	{
		if (_values[column][row] != value)
		{
			_values[column][row] = _itemType == "integer" ? value.toInt() : _itemType == "double" ? value.toDouble() : value;

			// the following triggers a reset of the view if the colwidth changes, but that is wrong. actually it should just change the cell-value.
			emit dataChanged(index(row, column), index(row, column), { Qt::DisplayRole });
			if (type != "formula") // For formula type, wait for the formulaCheckSucceeded signal before emitting modelChanged
				emit modelChanged();

			// Here we should *actually* check if specialRoles::maxColString changes and in that case: (so that the view can recalculate stuff)
			//	emit headerDataChanged(Qt::Orientation::Horizontal, column, column);
		}
	}
}

const Terms& ListModelTableViewBase::terms(const QString &what) const
{
	static Terms tempTerms;
	tempTerms.clear();

	int colNb = -1;
	if (what.isEmpty() && _values.length() == 1)
		colNb = 0;
	else if (!what.isEmpty())
	{
		colNb = _colNames.indexOf(what);
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
		if (_values.length() > colNb)
		{
			const QVector<QVariant> firstCol = _values[colNb];
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
	if(section < 0 || section >= (orientation == Qt::Horizontal ? _colNames.length() : _rowNames.length()))
		return QVariant();

	switch(role)
	{
	case int(specialRoles::maxColString): //A query from DataSetView for the maximumlength string to be expected! This to accomodate columnwidth
	{
		QString dummyText	= headerData(section, orientation, Qt::DisplayRole).toString() + "XXXXX";
		int colWidth		= getMaximumColumnWidthInCharacters(size_t(section));

		while(colWidth > dummyText.length())
			dummyText += "X";

		return dummyText;
	}
	case int(specialRoles::maxRowHeaderString):	return getMaximumRowHeaderString();
	case Qt::DisplayRole:						return QVariant(orientation == Qt::Horizontal ? _colNames[section] : _rowNames[section]);
	case Qt::TextAlignmentRole:					return QVariant(Qt::AlignCenter);
	default:									return QVariant();
	}
}

QHash<int, QByteArray> ListModelTableViewBase::roleNames() const
{
	static QHash<int, QByteArray> roles = ListModel::roleNames();

	static bool addRoles = true;

	if(addRoles)
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
	emit modelChanged();
}

void ListModelTableViewBase::modelChangedSlot()
{
	if (_boundTo)
	{
		std::vector<std::string> stdlevels;
		for (const QString& rowName : _rowNames)
			stdlevels.push_back(rowName.toStdString());

		std::vector<Options*> allOptions;

		for (int colIndex = 0; colIndex < _colNames.size(); colIndex++)
		{
			Options* options =		new Options();
			options->add("name",	new OptionString(_colNames[colIndex].toStdString()));
			options->add("levels",	new OptionVariables(stdlevels));

			std::vector<double> tempValues;
			for (QVariant val : _values[colIndex].toStdVector())
				tempValues.push_back(val.toDouble());
			options->add("values",	new OptionDoubleArray(tempValues));

			allOptions.push_back(options);
		}

		_boundTo->setValue(allOptions);
	}
}


OptionsTable *ListModelTableViewBase::createOption()
{
	Options* optsTemplate =		new Options();
	optsTemplate->add("name",	new OptionString());
	optsTemplate->add("levels", new OptionVariables());
	optsTemplate->add("values", new OptionDoubleArray());

	return new OptionsTable(optsTemplate);
}

void ListModelTableViewBase::initValues(OptionsTable * bindHere)
{
	_colNames.clear();
	_rowNames.clear();
	_values.clear();

	_boundTo = bindHere;

	std::vector<Options *>	options = bindHere->value();

	OptionVariables		* optionLevels = nullptr;

	for (Options * newRow : options)
	{
		OptionString		*	optionName		= static_cast<OptionString		*>(newRow->get("name"));
								optionLevels	= static_cast<OptionVariables	*>(newRow->get("levels")); // why not store it once?
		OptionDoubleArray	*	optionValues	= static_cast<OptionDoubleArray	*>(newRow->get("values"));

		_colNames.push_back(QString::fromStdString(optionName->value()));
		//levels = optionLevels->variables(); //The old code (in boundqmltableview.cpp) seemed to specify to simply use the *last* OptionVariables called "levels" in the binding option. So I'll just repeat that despite not getting it.
		_values.push_back({});
		for (double val : optionValues->value())
			_values[_values.size()-1].push_back(_itemType == "integer" ? round(val) : val);
	}

	if(optionLevels)
		for(const std::string & level : optionLevels->variables())
			_rowNames.push_back(QString::fromStdString(level));

	//No need to check colnames to cols in values because they are created during the same loop and thus crash if non-matching somehow
	if (_values.size() > 0 && int(_values[0].size()) != _rowNames.size())
		Log::log() << "Number of rows specifed in Options for ListModelTableViewBase does not match number of rows in values!" << std::endl;


	beginResetModel();

	_columnCount = _colNames.size();

	for(auto & col : _values)
		if(_rowNames.size() < col.size())
		{
			Log::log() << "Too many rows in a column of OptionsTable for ListModelTableViewBase! Shrinking column to fit." << std::endl;
			col.resize(_rowNames.size());
		}
		else
			for (int row = col.size(); row < _rowNames.size(); row++)
				col.push_back(1);

	//Ok, going to assume that the following: for (size_t i = values.size(); i < _columnCount; ++i) means we should add columns in case the data wasn't filled correctly (aka colNames did not match with values) but that cannot be now.

	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
}
