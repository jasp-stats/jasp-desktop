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
#include "listmodel.h"
#include "jasptheme.h"
#include "analysisform.h"
#include "analysisbase.h"
#include "jsonutilities.h"
#include "controls/sourceitem.h"
#include "controls/rowcontrols.h"
#include "controls/jasplistcontrol.h"

ListModel::ListModel(JASPListControl* listView) 
	: QAbstractTableModel(listView)
	,  VariableInfoConsumer()
	, _listView(listView)
{	
	// Connect all apecific signals to a general signal
	connect(this,	&ListModel::modelReset,				this,	&ListModel::termsChanged);
	connect(this,	&ListModel::rowsRemoved,			this,	&ListModel::termsChanged);
	connect(this,	&ListModel::rowsMoved,				this,	&ListModel::termsChanged);
	connect(this,	&ListModel::rowsInserted,			this,	&ListModel::termsChanged);
	connect(this,	&ListModel::dataChanged,			this,	&ListModel::dataChangedHandler);
	connect(this,	&ListModel::variableNamesChanged,	this,	&ListModel::termsChanged);
	connect(this,	&ListModel::variableTypeChanged,	this,	&ListModel::termsChanged);
}

QHash<int, QByteArray> ListModel::roleNames() const
{
	static QHash<int, QByteArray>	roles = QAbstractTableModel::roleNames();
	static bool						setMe = true;

	if(setMe)
	{
		roles[TypeRole]						= "type";
		roles[InfoRole]						= "info";
		roles[NameRole]						= "name";
		roles[ColumnPreviewRole]			= "preview";
		roles[SelectedRole]					= "selected";
		roles[SelectableRole]				= "selectable";
		roles[ColumnTypeRole]				= "columnType";
		roles[ColumnDescriptionRole]		= "description";
		roles[ColumnRealTypeRole]			= "columnRealType";
		roles[ColumnTypeDisabledIconRole]	= "columnTypeDisabledIcon";
		roles[ColumnTypeIconRole]			= "columnTypeIcon";
		roles[RowComponentRole]				= "rowComponent";
		roles[DeletableRole]				= "deletable";
		roles[VirtualRole]					= "virtual";

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

void ListModel::initTerms(const Terms &terms, const Terms::RelatedValuesPerTerm& allValuesMap)
{
    _initTerms(terms, allValuesMap, true);
}

void ListModel::_initTerms(const Terms &terms, const Terms::RelatedValuesPerTerm& allValuesMap, bool initRowControls)
{
	beginResetModel();
	if (initRowControls)
		// If some row controls are not used anymore, they will be removed during setUpRowControls
		_rowControlsValues = allValuesMap;

	_setTerms(terms);
	endResetModel();

	if (initRowControls)	
		_connectAllSourcesControls();
}

void ListModel::_connectAllSourcesControls()
{
	for (SourceItem* sourceItem : listView()->sourceItems())
		_connectSourceControls(sourceItem);
}

void ListModel::_setAllowedType(Term& term) const
{
	columnType type = term.type();
	if (type != columnType::unknown)
	{
		if (!_listView->isTypeAllowed(type))
			term.setType(_listView->defaultType());
		else
		{
			// If the real type is now allowed, change the type to its original one.
			columnType realType = getVariableRealType(term.value());
			if (type != realType && _listView->isTypeAllowed(realType))
				term.setType(realType);
		}
	}
}

Term ListModel::_checkTermType(const Term &term) const
{
	Term checkedTerm(term);
	_setAllowedType(checkedTerm);

	return checkedTerm;
}

Terms ListModel::checkTermsTypes(const std::vector<Term>& terms) const
{
	Terms checkedTerms;
	for (const Term& term : terms)
		checkedTerms.add(_checkTermType(term));

	return checkedTerms;
}

void ListModel::checkTermsTypes()
{
	_terms.set(checkTermsTypes(_terms));
	setUpRowControls();
}


Terms ListModel::checkTermsTypes(const Terms& terms) const
{
	Terms checkedTerms = terms; // Keep terms properties
	for (Term& term : checkedTerms)
		_setAllowedType(term);
	return checkedTerms;
}

void ListModel::_connectSourceControls(SourceItem* sourceItem)
{
	// Connect option changes from controls in sourceModel that influence the terms of this model:
	// either the source uses direclty a control in sourceModel (controlName()), or the source uses
	// a conditional expression.
	ListModel* sourceModel = sourceItem->sourceListModel();
	if (!sourceModel) 
		return;

	const Terms& terms = sourceModel->terms();
	if (terms.size() == 0) 
		return;

	for (const QString & controlName : sourceItem->usedControls())
		for (const Term & term : terms)
		{
			JASPControl * control = sourceModel->getRowControl(term.value(), controlName);
			if (control)
			{
				BoundControl * boundControl = control->boundControl();
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

Terms ListModel::getSourceTerms()
{
	Terms termsAvailable;

	listView()->applyToAllSources([&](SourceItem *sourceItem, const Terms& terms)
	{
		_connectSourceControls(sourceItem);
		termsAvailable.add(terms);
	});
	
	return termsAvailable;
}

void ListModel::setRowComponent(QQmlComponent* rowComponent)
{
	_rowComponent = rowComponent;
}

void ListModel::setUpRowControls(int startRow, bool onlyRemove)
{
	if (_rowComponent == nullptr)
		return;

	if (!onlyRemove)
	{
		int row = 0;
		for (const Term& term : terms())
		{
			if (startRow <= row)
			{
				if (!_rowControlsMap.contains(term.value()))
				{
					RowControls* rowControls = new RowControls(this, _rowComponent);
					_rowControlsMap[term.value()] = rowControls;
					rowControls->initValues(row, term, _rowControlsValues[term.value()]);
				}
				else
					_rowControlsMap[term.value()]->resetValues(row, term, _rowControlsValues[term.value()]);
			}

			row++;
		}
	}
	
	// Disconnect and delete all controls that are not used anymore
	QStringList removedKeys;
	for (const QString& key : _rowControlsMap.keys())
		if (!terms().containsValue(key))
		{
			_rowControlsMap[key]->disconnectAndDeleteControls();
			removedKeys.append(key);
		}

	for (const QString& key : removedKeys)
		_rowControlsMap.remove(key);
}

Terms::RelatedValuesPerTerm ListModel::getTermsWithComponentValues() const
{
	Terms::RelatedValuesPerTerm result;

	for (const Term& term : _terms)
	{
		QMap<QString, Json::Value> componentValues;
		RowControls* rowControls = _rowControlsMap.value(term.value());
		if (rowControls)
		{
			const QMap<QString, JASPControl*>& controlsMap = rowControls->getJASPControlsMap();
			QMapIterator<QString, JASPControl*> it(controlsMap);
			while (it.hasNext())
			{
				it.next();
				JASPControl* control = it.value();
				BoundControl* boundControl = control->boundControl();
				if (boundControl)
					componentValues[it.key()] = boundControl->boundValue();
			}
		}

		result[term.value()] = componentValues;
	}

	return result;
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

QStringList ListModel::getUsedTypes() const
{
	QSet<QString> types;

	for (const Term& term : terms())
	{
		columnType type = term.type();
		if (type != columnType::unknown)
			types.insert(tq(columnTypeToString(type)));
	}

	return types.values();
}

void ListModel::setVariableType(int ind, columnType type)
{
	if (ind < 0 || ind > _terms.size())
		return;

	Term& term = _terms.at(ind);
	if (term.type() == type)
		return;

	Term newTerm = term;
	newTerm.setType(type);

	if(newTerm.components().isEmpty())
		return;

	sourceVariableTypeChanged(newTerm.components()[0], newTerm.type());
}

columnType ListModel::getVariableType(const QString& value) const
{
	int i = terms().indexOfValue(value);
	if (i >= 0)
		return terms().at(i).type();

	return (columnType)requestInfo(varInfoType::VariableType, value).toInt();
}

QString ListModel::getVariableDescription(const QString &name) const
{
	return requestInfo(varInfoType::ColumnDescription, name).toString();
}

columnType ListModel::getVariableRealType(const QString& name) const
{
	return (columnType)requestInfo(varInfoType::VariableType, name).toInt();
}

QString ListModel::getVariablePreview(const QString& name) const
{
	columnType	chosenType	= getVariableType(name),
				realType	= getVariableRealType(name);
	
	if(chosenType == realType)
		return "";
	
	varInfoType		previewType;
	
	switch(chosenType)
	{
	default:					previewType = varInfoType::PreviewScale;		break;
	case columnType::ordinal:	previewType	= varInfoType::PreviewOrdinal;		break;
	case columnType::nominal:	previewType	= varInfoType::PreviewNominal;		break;
	}
	
	return requestInfo(previewType, name).toString();
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
			if (term.label().toLower().startsWith(searchStringLower))
			{
				result = int(index);
				break;
			}
		}
	}

	return result;
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
					changed = true;
					break;
				}
			}
			if (i == _selectedItems.length())
			{
				_selectedItems.append(_index);
				changed = true;
			}
		}
	}
	else
	{
		if (_selectedItems.removeAll(_index) > 0)
			changed = true;
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

void ListModel::setSelectedItemWithName(QString name)
{
	int i = 0;

	for (const Term& term : terms())
	{
		if (term.label() == name)
		{
			setSelectedItem(i);
			break;
		}
		i++;
	}
}

void ListModel::selectAllItems()
{
	int nbTerms = rowCount();
	if (nbTerms == 0) return;

	_selectedItems.clear();

	for (int i = 0; i < nbTerms; i++)
	{
		if (data(index(i, 0), ListModel::SelectableRole).toBool())
			_selectedItems.append(i);
	}

	emit dataChanged(index(0, 0), index(nbTerms - 1, 0), { ListModel::SelectedRole });
	emit selectedItemsChanged();
}

void ListModel::cleanUp()
{
	blockSignals(true);
}

//void ListModel::variableTypeChanged(QString columnName, columnType columnType)
//{
//	sourceVariableTypeChanged(Term(columnName, columnType));
//}

void ListModel::sourceTermsReset()
{
	_initTerms(getSourceTerms(), Terms::RelatedValuesPerTerm(), false);
}

int ListModel::rowCount(const QModelIndex &) const
{
	return int(terms().size());
}

Terms ListModel::termsFromIndexes(const QList<int> &indexes) const
{
	Terms result;

	for (int index : indexes)
		if (size_t(index) < terms().size())
			result.add(terms().at(index));

	return result;
}

QList<int> ListModel::indexesFromTerms(const  Terms & termsIn) const
{
	std::set<int>		result;
	std::map<Term, int> termToIndex;

	for(size_t t=0; t<terms().size(); t++)
		termToIndex[terms().at(t)] = t;

	for(const Term & term : termsIn)
		if(termToIndex.count(term))
			result.insert(termToIndex[term]);

	return tql(result);
}



QVariant ListModel::data(const QModelIndex &index, int role) const
{
	int row = index.row();
	Terms terms = termsFromIndexes({row});
	if (terms.size() == 0)
		return QVariant();

	const Term& term = terms[0];

	switch (role)
	{
	case Qt::DisplayRole:
	case ListModel::NameRole:			return term.label();
	case ListModel::InfoRole:			return term.info();
	case ListModel::SelectableRole:		return !term.value().isEmpty() && term.isDraggable();
	case ListModel::SelectedRole:		return _selectedItems.contains(row);
	case ListModel::TypeRole:			return listView()->containsVariables() ? "variable" : "";

	case ListModel::RowComponentRole:
	{
		QString termStr = term.value();
		return _rowControlsMap.contains(termStr) ? QVariant::fromValue(_rowControlsMap[termStr]->getRowObject()) : QVariant();
	}
		
	case ListModel::ColumnPreviewRole:
		return (!listView()->containsVariables() || term.size() != 1) ? "" : getVariablePreview(term.value());
		
	case ListModel::ColumnDescriptionRole:
		return (!listView()->containsVariables() || term.size() != 1) ? "" : getVariableDescription(term.value());
	
	case ListModel::ColumnTypeRole:
	case ListModel::ColumnRealTypeRole:
	case ListModel::ColumnTypeIconRole:
	case ListModel::ColumnTypeDisabledIconRole:
		if (!listView()->containsVariables() || term.size() != 1)	
			return "";
		else
		{
			columnType	colType		= getVariableType(term.value()),
						colRealType = getVariableRealType(term.value());
			
			switch(role)
			{
			case ListModel::ColumnTypeRole:								return columnTypeToQString(colType);
			case ListModel::ColumnRealTypeRole:							return columnTypeToQString(colRealType);
			case ListModel::ColumnTypeIconRole:							return colType == columnType::unknown ? "" : (JaspTheme::currentIconPath() + getIconFilename(colType, colType == colRealType ? varIconType::DefaultIconType : varIconType::TransformedIconType));
			case ListModel::ColumnTypeDisabledIconRole:					return colType == columnType::unknown ? "" : (JaspTheme::currentIconPath() + getIconFilename(colType, varIconType::DisabledIconType));
			}
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

	const static QString typeIs = "type=";
	const static QString controlIs = "control=";

	QString useTheseVariableTypes, useThisControl, discardIndexes;

	for (const QString& filter : filters)
	{
		if (filter.startsWith(typeIs))		useTheseVariableTypes	= filter.right(filter.length() - typeIs.length());
		if (filter.startsWith(controlIs))	useThisControl			= filter.right(filter.length() - controlIs.length());
	}

	if (!useThisControl.isEmpty())
	{
		Terms controlTerms;
		for (const Term& term : result)
		{
			RowControls* rowControls = _rowControlsMap.value(term.value());
			if (rowControls)
			{
				JASPControl* control = rowControls->getJASPControl(useThisControl);

				if (control)	controlTerms.add(control->property("value").toString());
				else			Log::log() << "Could not find control " << useThisControl << " in list view " << name() << std::endl;
			}
		}
		result = controlTerms;
	}

	if (!useTheseVariableTypes.isEmpty())
	{
		QStringList typesStr = useTheseVariableTypes.split("|");
		QList<columnType> types;

		for (const QString& typeStr : typesStr)
		{
			columnType type = columnTypeFromString(fq(typeStr), columnType::unknown);
			if (type != columnType::unknown)
				types.push_back(type);
		}

		Terms rightValues;
		for (const Term& term : result)
		{
			if (types.contains(term.type()))
				rightValues.add(term);
		}
		result = rightValues;
	}

	if (filters.contains("levels"))
		result = allLevels(result);

	return result;
}

QStringList	ListModel::allLevels(const Terms& terms) const
{
	QStringList result;
	for (const Term& term : terms)
		result.append(requestInfo(varInfoType::Labels, term.value()).toStringList());

	return result;
}

Terms ListModel::termsEx(const QStringList &filters)
{
	return filterTerms(terms(), filters);
}

void ListModel::sourceVariableNamesChanged(QMap<QString, QString> map)
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
		emit variableNamesChanged(changedNamesMap);
}

bool ListModel::sourceVariableTypeChanged(QString columnName, columnType colType)
{
	bool change = false;
	Term sourceTerm(columnName, colType);

	for (int i = 0; i < _terms.size(); i++)
	{
		Term& term = _terms.at(i);
		if (term.containsAll(sourceTerm))
		{
			// A term may have several components: if all sourceTerm's components are in the term's components,
			// then for each component in term that is in sourceTerm, replace its type (in term) by the one in sourceTerm
			columnTypeVec	types = term.types(),
							sourceTypes = sourceTerm.types();
			int				sourceInd = 0;

			for (const QString& sourceComponent : sourceTerm.components())
			{
				int ind = term.components().indexOf(sourceComponent);
				if (ind >= 0 && types.size() > ind && sourceTypes.size() > sourceInd)
					types[ind] = sourceTypes[sourceInd];
				sourceInd++;
			}
			term.setTypes(types);
			QModelIndex ind = index(i, 0);

			emit dataChanged(ind, ind, {ListModel::ColumnTypeRole, ListModel::ColumnTypeIconRole, ListModel::ColumnTypeDisabledIconRole, ListModel::ColumnPreviewRole});
			emit variableTypeChanged(columnName, colType);

			change = true;
		}
	}
	return change;
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
				if (terms().containsValue(it.key()))
				{
					change = true;
					newChangedValues[it.key()] = it.value();
				}
			}
			sourceVariableNamesChanged(newChangedValues);
		}
	}
	else
	{
		change = terms().containsValue(columnName);
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
		change = terms().containsValue(columnName);
		if (change)
			emit labelsReordered(columnName);
	}

	return change;
}

void ListModel::sourceVariablesChanged(QStringList columns)
{
	QStringList changedColumns;

	for (const QString& column : columns)
	{
		if (_columnsUsedForLabels.contains(column))
			sourceLabelsChanged(column);
		else if (terms().containsValue(column))
			changedColumns.push_back(column);
	}

	if (changedColumns.size() > 0)
	{
		for (const QString& col : changedColumns)
		{
			int i = terms().indexOfValue(col);
			emit dataChanged(index(1,0), index(i,0));
		}
		emit variablesChanged(changedColumns);

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

void ListModel::_setTerms(const Terms &terms)
{
	_terms.set(checkTermsTypes(terms));
	setUpRowControls();
}

void ListModel::_removeTerms(const Terms &terms)
{
	_terms.remove(terms);
	setUpRowControls(0, true);
}

void ListModel::_removeTerm(int index)
{
	_terms.remove(size_t(index));
	setUpRowControls(0, true);
}

void ListModel::_removeTerm(const Term &term)
{
	_terms.remove(term);
	setUpRowControls(0, true);
}

void ListModel::_removeLastTerm()
{
	if (_terms.size() > 0)
		_terms.remove(_terms.size() - 1);
	setUpRowControls(0, true);
}

void ListModel::_addTerms(const Terms &terms)
{
	int i = _terms.size();
	_terms.add(checkTermsTypes(terms));
	setUpRowControls(i);
}

void ListModel::_addTerm(const Term &term, bool isUnique)
{
	int i = _terms.size();
	_terms.add(_checkTermType(term), isUnique);
	setUpRowControls(i);
}

void ListModel::_replaceTerm(int index, const Term &term)
{
	_terms.replace(index, _checkTermType(term));
	setUpRowControls();
}

Json::Value ListModel::getVariableTypes(bool onlyChanged) const
{
	return getVariableTypes(_terms, onlyChanged);
}

Json::Value ListModel::getVariableTypes(const Terms& terms, bool onlyChanged) const
{
	if (onlyChanged && _listView->hasMandatoryType()) // Don't need to ask for the changed types: this avoids to add the type in formula when it is not necessary
		return JsonUtilities::vecToJsonArray(stringvec(terms.size(), columnTypeToString(columnType::unknown)));

	return terms.types(onlyChanged, this);
}
