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

#include "listmodel.h"
#include "jasplistcontrol.h"
#include "analysisform.h"
#include "boundcontrolterms.h"
#include "rowcontrols.h"
#include "sourceitem.h"
#include <boost/bind.hpp>
#include "log.h"

ListModel::ListModel(JASPListControl* listView) 
	: QAbstractTableModel(listView)
	, _listView(listView)
{
	// Connect all apecific signals to a general signal
	connect(this,	&ListModel::modelReset,				this,	&ListModel::termsChanged);
	connect(this,	&ListModel::rowsRemoved,			this,	&ListModel::termsChanged);
	connect(this,	&ListModel::rowsMoved,				this,	&ListModel::termsChanged);
	connect(this,	&ListModel::rowsInserted,			this,	&ListModel::termsChanged);
	connect(this,	&ListModel::dataChanged,			this,	&ListModel::dataChangedHandler);
	connect(this,	&ListModel::namesChanged,			this,	&ListModel::termsChanged);
	connect(this,	&ListModel::columnTypeChanged,		this,	&ListModel::termsChanged);
}

QHash<int, QByteArray> ListModel::roleNames() const
{
	static QHash<int, QByteArray>	roles = QAbstractTableModel::roleNames();
	static bool						setMe = true;

	if(setMe)
	{
		roles[TypeRole]						= "type";
		roles[SelectedRole]					= "selected";
		roles[SelectableRole]				= "selectable";
		roles[ColumnTypeRole]				= "columnType";
		roles[ColumnTypeIconRole]			= "columnTypeIcon";
		roles[ColumnTypeDisabledIconRole]	= "columnTypeDisabledIcon";
		roles[NameRole]						= "name";
		roles[RowComponentRole]				= "rowComponent";
		roles[ValueRole]					= "value";
		roles[VirtualRole]					= "virtual";
		roles[DeletableRole]				= "deletable";

		setMe = false;
	}

	return roles;
}

void ListModel::refresh()
{
	beginResetModel(); 
	endResetModel();
}

void ListModel::addControlError(const QString &error) const
{
	_listView->addControlError(error);
}

void ListModel::initTerms(const Terms &terms, const RowControlsValues& allValuesMap)
{
	_initTerms(terms, allValuesMap, true);
}

void ListModel::_initTerms(const Terms &terms, const RowControlsValues& allValuesMap, bool setupControlConnections)
{
	beginResetModel();
	_rowControlsValues = allValuesMap;
	_setTerms(terms);
	endResetModel();

	if (setupControlConnections) _connectAllSourcesControls();
}

void ListModel::_connectAllSourcesControls()
{
	for (SourceItem* sourceItem : listView()->sourceItems())
		_connectSourceControls(sourceItem->listModel(), sourceItem->usedControls());
}

void ListModel::_connectSourceControls(ListModel* sourceModel, const QSet<QString>& controls)
{
	// Connect option changes from controls in sourceModel that influence the terms of this model
	if (!sourceModel || controls.size() == 0) return;

	const Terms& terms = sourceModel->terms();

	for (const QString& controlName : controls)
	{
		for (const Term& term : terms)
		{
			JASPControl* control = sourceModel->getRowControl(term.asQString(), controlName);
			if (control)
			{
				BoundControl* boundControl = control->boundControl();
				if (boundControl && !_rowControlsConnected.contains(boundControl))
				{
					connect(control, &JASPControl::boundValueChanged, this, &ListModel::sourceTermsReset);
					_rowControlsConnected.push_back(boundControl);
				}
			}
			else
				Log::log() << "Cannot find control " << controlName << " in model " << name() << std::endl;
		}
	}
}

Terms ListModel::getSourceTerms()
{
	Terms termsAvailable;

	listView()->applyToAllSources([&](SourceItem *sourceItem, const Terms& terms)
	{
		_connectSourceControls(sourceItem->listModel(), sourceItem->usedControls());
		termsAvailable.add(terms);
	});
	
	return termsAvailable;
}

ListModel *ListModel::getSourceModelOfTerm(const Term &term)
{
	ListModel* result = nullptr;

	listView()->applyToAllSources([&](SourceItem *sourceItem, const Terms& terms)
	{
		if (terms.contains(term))
			result = sourceItem->listModel();
	});

	return result;
}

void ListModel::setRowComponent(QQmlComponent* rowComponent)
{
	_rowComponent = rowComponent;
}

void ListModel::setUpRowControls()
{
	if (_rowComponent == nullptr)
		return;

	QStringList keys;
	int row = 0;
	for (const Term& term : terms())
	{
		const QString& key = term.asQString();
		keys.append(key);
		if (!_rowControlsMap.contains(key))
		{
			bool hasOptions = _rowControlsValues.contains(key);
			RowControls* rowControls = new RowControls(this, _rowComponent, _rowControlsValues[key]);
			_rowControlsMap[key] = rowControls;
			rowControls->init(row, term, !hasOptions);
		}
		else
			_rowControlsMap[key]->setContext(row, key);
		row++;
	}

	for (const QString& key : _rowControlsMap.keys())
		if (!keys.contains(key))
			// If some row controls are not used anymore, if they use some sources, they must be disconnected from these sources
			// If a source changes and emits a signal, these controls should not be activated (cf. https://github.com/jasp-stats/jasp-test-release/issues/1786)
			_rowControlsMap[key]->disconnectControls();
}

JASPControl *ListModel::getRowControl(const QString &key, const QString &name) const
{
	JASPControl* control = nullptr;

	RowControls* rowControls = _rowControlsMap.value(key);
	if (rowControls)
	{
		const QMap<QString, JASPControl*>& controls = rowControls->getJASPControlsMap();
		if (controls.contains(name))
			control = controls[name];
	}

	return control;
}

bool ListModel::addRowControl(const QString &key, JASPControl *control)
{
	return _rowControlsMap.contains(key) ? _rowControlsMap[key]->addJASPControl(control) : false;
}

QStringList ListModel::termsTypes()
{
	QSet<QString> types;

	for (const Term& term : terms())
	{
		columnType type = columnType(requestInfo(VariableInfo::VariableType, term.asQString()).toInt());
		if (type != columnType::unknown)
			types.insert(tq(columnTypeToString(type)));
	}

	return types.values();
}

int ListModel::searchTermWith(QString searchString)
{
	int result = -1;
	const Terms& myTerms = terms();
	int startIndex = 0;
	if (_selectedItems.length() > 0)
	{
		startIndex = _selectedItems.first();
		if (searchString.length() == 1)
			startIndex++;
	}

	if (searchString.length() > 0)
	{
		QString searchStringLower = searchString.toLower();
		for (size_t i = 0; i < myTerms.size(); i++)
		{
			size_t index = (size_t(startIndex) + i) % myTerms.size();
			const Term& term = myTerms.at(index);
			if (term.asQString().toLower().startsWith(searchStringLower))
			{
				result = int(index);
				break;
			}
		}
	}

	return result;
}

void ListModel::_addSelectedItemType(int _index)
{
	QString type = data(index(_index, 0), ListModel::ColumnTypeRole).toString();
	if (!type.isEmpty())
		_selectedItemsTypes.insert(type);
}

void ListModel::selectItem(int _index, bool _select)
{
	bool changed = false;
	if (_select)
	{
		if (data(index(_index, 0), ListModel::SelectableRole).toBool())
		{
			int i = 0;
			for (; i < _selectedItems.length(); i++)
			{
				if (_selectedItems[i] == _index)
					break;
				else if (_selectedItems[i] > _index)
				{
					_selectedItems.insert(i, _index);
					_addSelectedItemType(_index);
					changed = true;
					break;
				}
			}
			if (i == _selectedItems.length())
			{
				_selectedItems.append(_index);
				_addSelectedItemType(_index);
				changed = true;
			}
		}
	}
	else
	{
		if (_selectedItems.removeAll(_index) > 0)
		{
			_selectedItemsTypes.clear();
			for (int i : _selectedItems)
			{
				QString type = data(index(i, 0), ListModel::ColumnTypeRole).toString();
				if (!type.isEmpty())
					_selectedItemsTypes.insert(type);
			}
			changed = true;
		}
	}

	if (changed)
	{
		emit dataChanged(index(_index, 0), index(_index, 0), { ListModel::SelectedRole });
		emit selectedItemsChanged();
	}
}

void ListModel::clearSelectedItems(bool emitSelectedChange)
{
	QList<int> selected = _selectedItems;

	_selectedItems.clear();
	_selectedItemsTypes.clear();

	for (int i : selected)
		emit dataChanged(index(i,0), index(i,0), { ListModel::SelectedRole });

	if (selected.length() > 0 && emitSelectedChange)
		emit selectedItemsChanged();
}

void ListModel::setSelectedItem(int _index)
{
	if (_selectedItems.size() == 1 && _selectedItems[0] == _index) return;

	clearSelectedItems(false);
	selectItem(_index, true);
}

void ListModel::selectAllItems()
{
	int nbTerms = rowCount();
	if (nbTerms == 0) return;

	_selectedItems.clear();
	_selectedItemsTypes.clear();

	for (int i = 0; i < nbTerms; i++)
	{
		if (data(index(i, 0), ListModel::SelectableRole).toBool())
		{
			_selectedItems.append(i);
			_addSelectedItemType(i);
		}
	}

	emit dataChanged(index(0, 0), index(nbTerms - 1, 0), { ListModel::SelectedRole });
	emit selectedItemsChanged();
}

void ListModel::sourceTermsReset()
{
	_initTerms(getSourceTerms(), RowControlsValues(), false);
}

int ListModel::rowCount(const QModelIndex &) const
{
	return int(terms().size());
}

QVariant ListModel::data(const QModelIndex &index, int role) const
{
	int row = index.row();
	const Terms& myTerms = terms();
	size_t row_t = size_t(row);
	if (row_t >= myTerms.size())
		return QVariant();

	switch (role)
	{
	case Qt::DisplayRole:
	case ListModel::NameRole:			return QVariant(myTerms.at(row_t).asQString());
	case ListModel::SelectableRole:		return !myTerms.at(row_t).asQString().isEmpty();
	case ListModel::SelectedRole:		return _selectedItems.contains(row);
	case ListModel::RowComponentRole:
	{
		QString term = myTerms.at(row_t).asQString();
		return _rowControlsMap.contains(term) ? QVariant::fromValue(_rowControlsMap[term]->getRowObject()) : QVariant();
	}
	case ListModel::TypeRole:			return listView()->containsVariables() ? "variable" : "";
	case ListModel::ColumnTypeRole:
	case ListModel::ColumnTypeIconRole:
	case ListModel::ColumnTypeDisabledIconRole:
	{
		const Term& term = myTerms.at(row_t);
		if (!listView()->containsVariables() || term.size() != 1)	return "";
		if (role == ListModel::ColumnTypeRole)						return requestInfo(VariableInfo::VariableTypeName, term.asQString());
		else if (role == ListModel::ColumnTypeIconRole)				return requestInfo(VariableInfo::VariableTypeIcon, term.asQString());
		else if (role == ListModel::ColumnTypeDisabledIconRole)		return requestInfo(VariableInfo::VariableTypeDisabledIcon, term.asQString());
		break;
	}
	}

	return QVariant();
}

const QString &ListModel::name() const
{
	return _listView->name();
}

Terms ListModel::filterTerms(const Terms& terms, const QStringList& filters)
{
	if (filters.empty())	return terms;

	Terms result = terms;

	const QString typeIs = "type=";
	const QString controlIs = "control=";

	QString useTheseVariableTypes, useThisControl;

	for (const QString& filter : filters)
	{
		if (filter.startsWith(typeIs))		useTheseVariableTypes	= filter.right(filter.length() - typeIs.length());
		if (filter.startsWith(controlIs))	useThisControl			= filter.right(filter.length() - controlIs.length());
	}

	if (!useTheseVariableTypes.isEmpty())
	{
		result.clear();
		QStringList typesStr = useTheseVariableTypes.split("|");
		QList<columnType> types;

		for (const QString& typeStr : typesStr)
		{
			columnType type = columnTypeFromString(fq(typeStr), columnType::unknown);
			if (type != columnType::unknown)
				types.push_back(type);
		}

		for (const Term& term : terms)
		{
			columnType type = columnType(requestInfo(VariableInfo::VariableType, term.asQString()).toInt());
			if (types.contains(type))
				result.add(term);
		}
	}

	if (!useThisControl.isEmpty())
	{
		Terms controlTerms;
		for (const Term& term : result)
		{
			RowControls* rowControls = _rowControlsMap.value(term.asQString());
			if (rowControls)
			{
				JASPControl* control = rowControls->getJASPControl(useThisControl);

				if (control)	controlTerms.add(control->property("value").toString());
				else			Log::log() << "Could not find control " << useThisControl << " in list view " << name() << std::endl;
			}
		}
		result = controlTerms;
	}

	if (filters.contains("levels"))
	{
		Terms allLabels;
		for (const Term& term : result)
		{
			Terms labels = requestInfo(VariableInfo::Labels, term.asQString()).toStringList();
			if (labels.size() > 0)	allLabels.add(labels);
			else					allLabels.add(term);
		}

		result = allLabels;
	}

	return result;
}

Terms ListModel::termsEx(const QStringList &filters)
{
	return filterTerms(terms(), filters);
}

void ListModel::sourceNamesChanged(QMap<QString, QString> map)
{
	QMap<QString, QString>	changedNamesMap;
	QSet<int>				changedIndexes;

	QMapIterator<QString, QString> it(map);
	while (it.hasNext())
	{
		it.next();
		const QString& oldName = it.key(), newName = it.value();
		QSet<int> indexes = _terms.replaceVariableName(oldName.toStdString(), newName.toStdString());
		if (indexes.size() > 0)
		{
			changedNamesMap[oldName] = newName;
			changedIndexes += indexes;
		}
	}

	for (int i : changedIndexes)
	{
		QModelIndex ind = index(i, 0);
		emit dataChanged(ind, ind);
	}

	if (changedNamesMap.size() > 0)
		emit namesChanged(changedNamesMap);
}

int ListModel::sourceColumnTypeChanged(QString name)
{
	int i = terms().indexOf(name);
	if (i >= 0)
	{
		QModelIndex ind = index(i, 0);
		emit dataChanged(ind, ind);
		emit columnTypeChanged(name);
	}

	return i;
}

bool ListModel::sourceLabelsChanged(QString columnName, QMap<QString, QString> changedLabels)
{
	bool change = false;
	if (_columnsUsedForLabels.contains(columnName))
	{
		if (changedLabels.size() == 0)
		{
			// The changed labels are not specified. Requery the source.
			sourceTermsReset();
			change = true;
		}
		else
		{
			QMap<QString, QString> newChangedValues;
			QMapIterator<QString, QString> it(changedLabels);
			while (it.hasNext())
			{
				it.next();
				if (terms().contains(it.key()))
				{
					change = true;
					newChangedValues[it.key()] = it.value();
				}
			}
			sourceNamesChanged(newChangedValues);
		}
	}
	else
	{
		change = terms().contains(columnName);
		if (change)
			emit labelsChanged(columnName, changedLabels);
	}

	return change;
}

bool ListModel::sourceLabelsReordered(QString columnName)
{
	bool change = false;
	if (_columnsUsedForLabels.contains(columnName))
	{
		sourceTermsReset();
		change = true;
	}
	else
	{
		change = terms().contains(columnName);
		if (change)
			emit labelsReordered(columnName);
	}

	return change;
}

void ListModel::sourceColumnsChanged(QStringList columns)
{
	QStringList changedColumns;

	for (const QString& column : columns)
	{
		if (_columnsUsedForLabels.contains(column))
			sourceLabelsChanged(column);
		else if (terms().contains(column))
			changedColumns.push_back(column);
	}

	if (changedColumns.size() > 0)
	{
		emit columnsChanged(changedColumns);

		if (listView()->isBound())
			listView()->form()->refreshAnalysis();
	}
}

void ListModel::dataChangedHandler(const QModelIndex &, const QModelIndex &, const QVector<int> &roles)
{
	if (roles.isEmpty() || roles.size() > 1 || roles[0] != ListModel::SelectedRole)
		emit termsChanged();
}

void ListModel::_setTerms(const Terms &terms, const Terms& parentTerms)
{
	_terms.removeParent();
	_setTerms(terms);
	_terms.setSortParent(parentTerms);
}

void ListModel::_setTerms(const std::vector<Term> &terms)
{
	_terms.set(terms);
	setUpRowControls();
}

void ListModel::_setTerms(const Terms &terms)
{
	_terms.set(terms);
	setUpRowControls();
}

void ListModel::_removeTerms(const Terms &terms)
{
	_terms.remove(terms);
	setUpRowControls();
}

void ListModel::_removeTerm(int index)
{
	_terms.remove(size_t(index));
	setUpRowControls();
}

void ListModel::_removeTerm(const Term &term)
{
	_terms.remove(term);
	setUpRowControls();
}

void ListModel::_removeLastTerm()
{
	if (_terms.size() > 0)
		_terms.remove(_terms.size() - 1);
}

void ListModel::_addTerms(const Terms &terms)
{
	_terms.add(terms);
	setUpRowControls();
}

void ListModel::_addTerm(const QString &term, bool isUnique)
{
	_terms.add(term, isUnique);
	setUpRowControls();
}

void ListModel::_replaceTerm(int index, const Term &term)
{
	_terms.replace(index, term);
	setUpRowControls();
}
