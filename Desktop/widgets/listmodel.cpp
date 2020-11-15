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
#include "../analysis/analysisform.h"
#include "boundcontrolterms.h"
#include "rowcontrols.h"
#include "../analysis/jaspcontrol.h"
#include "jasplistcontrol.h"
#include <QJSEngine>
#include <boost/bind.hpp>
#include "log.h"

ListModel::ListModel(JASPListControl* listView) 
	: QAbstractTableModel(listView)
	, _listView(listView)
{
	setInfoProvider(listView->form());
}

QHash<int, QByteArray> ListModel::roleNames() const
{
	static QHash<int, QByteArray>	roles = QAbstractTableModel::roleNames();
	static bool						setMe = true;

	if(setMe)
	{
		roles[TypeRole]				= "type";
		roles[SelectedRole]			= "selected";
		roles[SelectableRole]		= "selectable";
		roles[ColumnTypeRole]		= "columnType";
		roles[NameRole]				= "name";
		roles[RowComponentRole]		= "rowComponent";
		roles[ValueRole]			= "value";

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

void ListModel::initTerms(const Terms &terms, const RowControlsOptions& allOptionsMap)
{
	_initTerms(terms, allOptionsMap, true);
}

void ListModel::_initTerms(const Terms &terms, const RowControlsOptions& allOptionsMap, bool setupControlConnections)
{
	beginResetModel();
	_terms.set(terms);
	_rowControlsOptions = allOptionsMap;
	endResetModel();

	if (setupControlConnections)
		for (JASPListControl::SourceType* sourceType : listView()->sourceModels())
			_connectSourceControls(sourceType->model, sourceType->usedControls);
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
			BoundControl* boundControl = dynamic_cast<BoundControl*>(control);
			if (boundControl && !_rowControlsConnected.contains(boundControl))
			{
				boundControl->boundTo()->changed.connect(boost::bind(&ListModel::_sourceTermsChangedHandler, this, _1));
				_rowControlsConnected.push_back(boundControl);
			}
		}
	}
}

Terms ListModel::getSourceTerms()
{
	Terms termsAvailable;
	Terms termsToCombine;
	Terms termsToBeCombinedWith;

	for (const std::pair<JASPListControl::SourceType *, Terms>& source : listView()->getTermsPerSource())
	{
		JASPListControl::SourceType* sourceItem = source.first;
		const Terms& sourceTerms = source.second;
		ListModel* sourceModel = sourceItem->model;
		if (sourceModel)
		{
			_connectSourceControls(sourceModel, sourceItem->usedControls);

			if (sourceItem->combineWithOtherModels)
				termsToCombine = sourceTerms;
			else
				termsToBeCombinedWith.add(sourceTerms);

			termsAvailable.add(sourceTerms);
		}
	}

	if (termsToCombine.size() > 0)
	{
		for (const Term& termToCombine : termsToCombine)
		{
			for (const Term& termToBeCombined : termsToBeCombinedWith)
			{
				QStringList components = termToCombine.components();
				components.append(termToBeCombined.components());
				termsAvailable.add(Term(components));
			}
		}
	}
	
	return termsAvailable;
}

ListModel *ListModel::getSourceModelOfTerm(const Term &term)
{
	ListModel* result = nullptr;

	for (const std::pair<JASPListControl::SourceType *, Terms>& source : listView()->getTermsPerSource())
	{
		if (source.second.contains(term))
			result = source.first->model;
	}
	return result;
}

void ListModel::setRowComponent(QQmlComponent* rowComponent)
{
	_rowComponent = rowComponent;
}

void ListModel::endResetModel()
{
	setUpRowControls();
	QAbstractTableModel::endResetModel();
}

void ListModel::setUpRowControls()
{
	if (_rowComponent == nullptr)
		return;

	int row = 0;
	for (const Term& term : terms())
	{
		const QString& key = term.asQString();
		if (!_rowControlsMap.contains(key))
		{
			bool hasOptions = _rowControlsOptions.contains(key);
			RowControls* rowControls = new RowControls(this, _rowComponent, _rowControlsOptions[key]);
			_rowControlsMap[key] = rowControls;
			rowControls->init(row, term, !hasOptions);
		}
		else
			_rowControlsMap[key]->setContext(row, key);
		row++;
	}
}

JASPControl *ListModel::getRowControl(const QString &key, const QString &name) const
{
	JASPControl* control = nullptr;

	if (_rowControlsMap.contains(key))
	{
		RowControls* rowControls = _rowControlsMap[key];
		const QMap<QString, JASPControl*>& controls = rowControls->getJASPControlsMap();
		if (controls.contains(name))
			control = controls[name];
	}

	return control;
}

bool ListModel::addRowControl(const QString &key, JASPControl *control)
{
	bool success = false;

	if (_rowControlsMap.contains(key))
		success = _rowControlsMap[key]->addJASPControl(control);

	return success;
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

void ListModel::_sourceTermsChangedHandler(Option *)
{
	sourceTermsChanged(nullptr, nullptr);
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
		emit dataChanged(index(_index, 0), index(_index, 0));
		emit selectedItemsChanged();
	}
}

void ListModel::clearSelectedItems(bool emitSelectedChange)
{
	QList<int> selected = _selectedItems;

	_selectedItems.clear();
	_selectedItemsTypes.clear();

	for (int i : selected)
		emit dataChanged(index(i,0), index(i,0));

	if (selected.length() > 0 && emitSelectedChange)
		emit selectedItemsChanged();
}

void ListModel::setSelectedItem(int _index)
{
	clearSelectedItems(false);
	selectItem(_index, true);
}

void ListModel::selectAllItems()
{
	int nbTerms = int(terms().size());
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

	emit dataChanged(index(0, 0), index(nbTerms - 1, 0));
	emit selectedItemsChanged();
}

QList<QString> ListModel::itemTypes()
{
	QSet<QString> types;

	for (const Term& term : terms())
	{
		columnType type = DataSetPackage::pkg()->getColumnType(term.asString());
		if (type != columnType::unknown)
			types.insert(columnTypeToQString(type));
	}

	return types.values();
}


void ListModel::sourceTermsChanged(const Terms *termsAdded, const Terms *termsRemoved)
{
	_initTerms(getSourceTerms(), RowControlsOptions(), false);
	
	emit modelChanged(termsAdded, termsRemoved);
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

	if (role == Qt::DisplayRole || role == ListModel::NameRole)
	{
		const Term& term = myTerms.at(row_t);
		return QVariant(term.asQString());
	}
	if (role == ListModel::SelectableRole)
		return !myTerms.at(row_t).asQString().isEmpty();
	if (role == ListModel::SelectedRole)
	{
		if (_selectedItems.contains(row))
			return true;
		else
			return false;
	}
	if (role == ListModel::RowComponentRole)
	{
		if (_rowControlsMap.size() > 0)
			return QVariant::fromValue(_rowControlsMap[myTerms.at(row_t).asQString()]->getRowObject());
		else
			return QVariant();
	}
	
	if (!areTermsVariables())
		return QVariant();
	
	if (role == ListModel::TypeRole)
		return QVariant("variable");
	else if (role == ListModel::ColumnTypeRole)
	{
		const Term& term = myTerms.at(row_t);
		if (term.size() != 1)
			return QVariant();

		return requestInfo(term, VariableInfo::VariableTypeName);
	}
	
	return QVariant();
}

const QString &ListModel::name() const
{
	return _listView->name();
}

const Terms &ListModel::terms(const QString &what) const
{
	const QString typeIs = "type=";
	const QString controlIs = "control=";

	if (!what.startsWith(controlIs) && !what.startsWith(typeIs))
		return _terms; // in most cases, it comes here.

	// This method was defined as const (it is used in other const methods), and returns a reference to a const Terms
	// It returns 99% of the cases _terms
	// But in case that a condition is used (what is not empty), then it cannot return a local variable nor a member of this class
	// So a static variable is used.
	static Terms specialTerms;

	QStringList allConditions = what.split(",");
	QString useTheseVariableTypes, useThisControl;

	for (const QString& condition : allConditions)
	{
		if (condition.startsWith(typeIs))		useTheseVariableTypes	= condition.right(condition.length() - typeIs.length());
		if (condition.startsWith(controlIs))	useThisControl			= condition.right(condition.length() - controlIs.length());
	}

	if (!useTheseVariableTypes.isEmpty())
	{
		specialTerms.clear();

		QStringList typesStr = useTheseVariableTypes.split("|");
		QList<columnType> types;

		for (const QString& typeStr : typesStr)
		{
			columnType type = columnTypeFromQString(typeStr, columnType::unknown);
			if (type != columnType::unknown)
				types.push_back(type);
		}

		for (const Term& term : _terms)
		{
			columnType type = DataSetPackage::pkg()->getColumnType(term.asString());
			if (types.contains(type))
				specialTerms.add(term);
		}
	}
	else
		specialTerms = _terms;

	if (!useThisControl.isEmpty())
	{
		Terms controlTerms;
		for (const Term& term : specialTerms)
		{
			RowControls* rowControls = _rowControlsMap[term.asQString()];
			if (rowControls)
			{
				JASPControl* control = rowControls->getJASPControl(useThisControl);

				if (control)	controlTerms.add(control->property("value").toString());
				else			Log::log() << "Could not find control " << useThisControl << " in list view " << name() << std::endl;
			}
		}
		specialTerms = controlTerms;
	}

	return specialTerms;
}

void ListModel::setTermsAreVariables(bool areVariables)
{
	_areTermsVariables = areVariables;
	if (!areVariables)
		listView()->setProperty("showVariableTypeIcon", false);
}

void ListModel::replaceVariableName(const std::string & oldName, const std::string & newName)
{
	_terms.replaceVariableName(oldName, newName);

	for(const QString & key : _rowControlsOptions.keys())
		for(const QString & key2 : _rowControlsOptions[key].keys())
			_rowControlsOptions[key][key2]->replaceVariableName(oldName, newName);
}
