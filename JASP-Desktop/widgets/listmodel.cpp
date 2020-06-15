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
#include "boundqmllistviewterms.h"
#include "rowcontrols.h"
#include "../analysis/jaspcontrolbase.h"
#include <QJSEngine>
#include <boost/bind.hpp>
#include "log.h"

ListModel::ListModel(QMLListView* listView) 
	: QAbstractTableModel(listView)
	, _listView(listView)
{
	setInfoProvider(listView->form());
	_areTermsVariables = true;
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
		roles[RowComponentsRole]	= "rowComponents";
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

	if (setupControlConnections && listView()->sourceModels().size() > 0)
	{
		QMap<ListModel*, Terms> map = getSourceTermsPerModel();
		QMapIterator<ListModel*, Terms> it(map);

		while (it.hasNext())
		{
			it.next();
			ListModel* sourceModel = it.key();
			const Terms& terms = it.value();
			QMLListView::SourceType* sourceType = listView()->getSourceTypeFromModel(sourceModel);
			if (sourceType)
			{
				for (const QMLListView::SourceType::ConditionVariable& conditionVariable : sourceType->conditionVariables)
					for (const Term& term : terms)
						_connectControl(sourceModel->getRowControl(term.asQString(), conditionVariable.controlName));
			}
		}
	}
}

void ListModel::_connectControl(JASPControlWrapper* control)
{
	BoundQMLItem* boundControl = dynamic_cast<BoundQMLItem*>(control);
	if (boundControl && !_rowControlsConnected.contains(boundControl))
	{
		boundControl->boundTo()->changed.connect(boost::bind(&ListModel::_rowControlOptionChangedHandler, this, _1));
		_rowControlsConnected.push_back(boundControl);
	}
}

Terms ListModel::getSourceTerms()
{
	const QList<QMLListView::SourceType*>& sourceItems = listView()->sourceModels();

	Terms termsAvailable;
	if (sourceItems.size() == 0)
		return termsAvailable;

	Terms termsToCombine;
	Terms termsToBeCombinedWith;

	for (QMLListView::SourceType* sourceItem : sourceItems)
	{
		ListModel* sourceModel = sourceItem->model;
		if (sourceModel)
		{
			Terms sourceTerms = sourceModel->terms(sourceItem->modelUse);

			for (const QMLListView::SourceType& discardModel : sourceItem->getDiscardModels())
				sourceTerms.discardWhatDoesContainTheseComponents(discardModel.model->terms(discardModel.modelUse));

			if (!sourceItem->conditionExpression.isEmpty())
			{
				Terms filteredTerms;
				QJSEngine jsEngine;

				for (const Term& term : sourceTerms)
				{
					for (const QMLListView::SourceType::ConditionVariable& conditionVariable : sourceItem->conditionVariables)
					{
						JASPControlWrapper* control = sourceModel->getRowControl(term.asQString(), conditionVariable.controlName);
						if (control)
						{
							QJSValue value;
							QVariant valueVar = control->getItemProperty(conditionVariable.propertyName);

							switch (valueVar.type())
							{
							case QVariant::Type::Int:
							case QVariant::Type::UInt:		value = valueVar.toInt();		break;
							case QVariant::Type::Double:	value = valueVar.toDouble();	break;
							case QVariant::Type::Bool:		value = valueVar.toBool();		break;
							default:						value = valueVar.toString();	break;
							}

							jsEngine.globalObject().setProperty(conditionVariable.name, value);
							_connectControl(control);
						}
					}

					QJSValue result = jsEngine.evaluate(sourceItem->conditionExpression);
					if (result.isError())
							addControlError("Error when evaluating : " + sourceItem->conditionExpression + ": " + result.toString());
					else if (result.toBool())
						filteredTerms.add(term);
				}

				sourceTerms = filteredTerms;
			}

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

QMap<ListModel*, Terms> ListModel::getSourceTermsPerModel()
{
	QMap<ListModel*, Terms> result;
	const QList<QMLListView::SourceType*>& sourceItems = listView()->sourceModels();

	if (sourceItems.size() == 0)
		return result;

	for (QMLListView::SourceType* sourceItem : sourceItems)
	{
		ListModel* sourceModel = sourceItem->model;
		if (sourceModel)
		{
			Terms terms = sourceModel->terms(sourceItem->modelUse);

			for (const QMLListView::SourceType& discardModel : sourceItem->getDiscardModels())
				terms.discardWhatDoesContainTheseComponents(discardModel.model->terms(discardModel.modelUse));

			result[sourceModel] = terms;
		}
	}

	return result;
}

ListModel *ListModel::getSourceModelOfTerm(const Term &term)
{
	ListModel* result = nullptr;
	QMap<ListModel*, Terms> map = getSourceTermsPerModel();
	QMapIterator<ListModel*, Terms> it(map);
	while (it.hasNext())
	{
		it.next();
		if (it.value().contains(term))
			result = it.key();
	}
	return result;
}

void ListModel::setRowComponents(QList<QQmlComponent *> &rowComponents)
{
	_rowComponents = rowComponents;
}

void ListModel::endResetModel()
{
	setUpRowControls();
	QAbstractTableModel::endResetModel();
}

void ListModel::setUpRowControls()
{
	if (_rowComponents.empty())
		return;

	int row = 0;
	for (const Term& term : terms())
	{
		const QString& key = term.asQString();
		if (!_rowControlsMap.contains(key))
		{
			bool hasOptions = _rowControlsOptions.contains(key);
			RowControls* rowControls = new RowControls(this, _rowComponents, _rowControlsOptions[key]);
			_rowControlsMap[key] = rowControls;
			rowControls->init(row, term, !hasOptions);
		}
		else
			_rowControlsMap[key]->setContext(row, key);
		row++;
	}
}

JASPControlWrapper *ListModel::getRowControl(const QString &key, const QString &name) const
{
	JASPControlWrapper* control = nullptr;

	if (_rowControlsMap.contains(key))
	{
		RowControls* rowControls = _rowControlsMap[key];
		const QMap<QString, JASPControlWrapper*>& controls = rowControls->getJASPControlsMap();
		if (controls.contains(name))
			control = controls[name];
	}

	return control;
}

bool ListModel::addRowControl(const QString &key, JASPControlWrapper *control)
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

void ListModel::_rowControlOptionChangedHandler(Option *)
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
	if (role == ListModel::RowComponentsRole)
	{
		if (_rowControlsMap.size() > 0)
			return QVariant::fromValue(_rowControlsMap[myTerms.at(row_t).asQString()]->getObjects());
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

	if (what.startsWith(typeIs))
	{
		static Terms terms;

		QStringList typesStr = what.right(what.length() - typeIs.length()).split("|");
		QList<columnType> types;

		for (const QString& typeStr : typesStr)
		{
			columnType type = columnTypeFromQString(typeStr, columnType::unknown);
			if (type != columnType::unknown)
				types.push_back(type);
		}

		terms.clear();
		for (const Term& term : _terms)
		{
			columnType type = DataSetPackage::pkg()->getColumnType(term.asString());
			if (types.contains(type))
				terms.add(term);
		}

		return terms;
	}

	return _terms;
}

void ListModel::replaceVariableName(const std::string & oldName, const std::string & newName)
{
	_terms.replaceVariableName(oldName, newName);

	for(const QString & key : _rowControlsOptions.keys())
		for(const QString & key2 : _rowControlsOptions[key].keys())
			_rowControlsOptions[key][key2]->replaceVariableName(oldName, newName);
}

void ListModel::readModelProperty(QMLListView* item)
{
	QVariant modelVar = item->getItemProperty("values");

	if (modelVar.isNull())
	{
		if (item->getItemProperty("source").isNull())
			item->setModelHasAllVariables(true);
	}
	else
	{
		Terms terms;
		QList<QVariant> list = modelVar.toList();
		if (!list.isEmpty())
		{
			for (const QVariant& itemVariant : list)
				terms.add(itemVariant.toString());
		}
		else
		{
			QAbstractItemModel *srcModel = qobject_cast<QAbstractItemModel *>(modelVar.value<QObject *>());
			if (srcModel)
			{
				for (int i = 0; i < srcModel->rowCount(); i++)
					terms.add(srcModel->data(srcModel->index(i, 0)).toString());
			}
			else
				Log::log() << "Could not read model of " << name() << std::endl;
		}

		initTerms(terms);
	}
}
