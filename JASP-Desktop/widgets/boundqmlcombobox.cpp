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

#include "boundqmlcombobox.h"
#include "../analysis/analysisform.h"
#include <QQmlProperty>
#include <QQuickItem>
#include <QAbstractListModel>


BoundQMLComboBox::BoundQMLComboBox(QQuickItem* item, AnalysisForm* form) 
	: QMLItem(item, form)
	, QMLListView(item, form)
	, BoundQMLItem()
{
	initComboBox();
}

BoundQMLComboBox::BoundQMLComboBox(QMap<QString, QVariant> &properties, AnalysisForm *form)
	: QMLItem(properties, form)
	, QMLListView(nullptr, form)
	, BoundQMLItem()

{
	initComboBox();
}

void BoundQMLComboBox::initComboBox()
{
	_currentIndex = getItemProperty("currentIndex").toInt();
	bool addEmptyValue = getItemProperty("addEmptyValue").toBool();
	
	_model = new ListModelTermsAvailable(this);
	if (addEmptyValue)
		_model->addEmptyValue();

	QVariant model = getItemProperty("model");
	if (model.isNull())
		model = getItemProperty("values");

	if (model.isNull())
	{
		if (sourceModels().isEmpty())
			hasAllVariablesModel = true;
	}
	else
	{
		QString textRole = getItemProperty("textRole").toString();
		QString valueRole = getItemProperty("valueRole").toString();
		_model->setTermsAreVariables(false);
		Terms terms;
		QList<QVariant> list = model.toList();
		if (!list.isEmpty())
		{
			for (const QVariant& itemVariant : list)
			{
				QMap<QString, QVariant> labelValueMap = itemVariant.toMap();
				if (labelValueMap.isEmpty())
					terms.add(itemVariant.toString());
				else
				{
					QString key = labelValueMap[textRole].toString();
					QString value = labelValueMap[valueRole].toString();
					terms.add(key);
					_keyToValueMap[key] = value;
					_valueToKeyMap[value] = key;					
				}
			}
			_model->initTerms(terms);
		}
		else
		{
			QAbstractListModel *srcModel = qobject_cast<QAbstractListModel *>(model.value<QObject *>());
			if (srcModel)
			{
				QMap<QString, int> roleMap;
				QHash<int, QByteArray> roles = srcModel->roleNames();
				QHashIterator<int, QByteArray> i(roles);
				while (i.hasNext()) 
				{
					i.next();
					QString valueStr = QString::fromStdString(i.value().toStdString());
					roleMap[valueStr] = i.key();
				}
				for (int i = 0; i < srcModel->rowCount(); i++)
				{
					QModelIndex ind(srcModel->index(i));
					QString key = srcModel->data(ind, roleMap[textRole]).toString();
					QString value = srcModel->data(ind, roleMap[valueRole]).toString();
					terms.add(key);
					_keyToValueMap[key] = value;
					_valueToKeyMap[value] = key;
				}
				_model->initTerms(terms);
			}
			else
			{
				addError(QString::fromLatin1("Wrong kind of model specified in ComboBox ") + name());
			}
		}
	}
	
	_resetItemWidth();

	if (_item)
		QQuickItem::connect(_item, SIGNAL(activated(int)), this, SLOT(comboBoxChangeValueSlot(int)));
}

void BoundQMLComboBox::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionList *>(option);

	if (_boundTo != nullptr)
	{
		QString selectedValue = QString::fromStdString(_boundTo->value());
		int index = -1;
		QList<QString> values = _model->terms().asQList();
		if (values.size() > 0)
		{
			if (selectedValue.isEmpty())
				index = 0;
			else
			{
				if (_valueToKeyMap.contains(selectedValue))
					selectedValue = _valueToKeyMap[selectedValue];
				index = values.indexOf(selectedValue);
				if (index == -1)
				{
					addError(QString::fromLatin1("Unknown option ") + selectedValue + " in ComboBox " + name());
					index = 0;
				}
			}
		}
		std::vector<std::string> options = _getOptionValues();
		_boundTo->resetOptions(options, index);
		
		_setCurrentValue(index, true, false);
		
		_resetItemWidth();
	}
	else
		addError(QString::fromLatin1("Unknown error in ComboBox ") + name());
}

void BoundQMLComboBox::resetQMLItem(QQuickItem *item)
{
	BoundQMLItem::resetQMLItem(item);
	
	setItemProperty("model", QVariant::fromValue(_model));
	setItemProperty("currentIndex", _currentIndex);
	setItemProperty("currentText", _currentText);
	setItemProperty("currentColumnType", _currentColumnType);
	setItemProperty("initialized", true);
	_resetItemWidth();

	if (_item)
		QQuickItem::connect(_item, SIGNAL(activated(int)), this, SLOT(comboBoxChangeValueSlot(int)));
}

std::vector<std::string> BoundQMLComboBox::_getOptionValues()
{
	std::vector<std::string> options;
	const Terms& terms = _model->terms();
	for (const Term& term : terms)
	{
		QString val = term.asQString();
		if (_keyToValueMap.contains(val))
			val = _keyToValueMap[val];
		options.push_back(val.toStdString());
	}
	
	return options;
}

Option *BoundQMLComboBox::createOption()
{
	std::vector<std::string> options = _getOptionValues();
	
	int index = getItemProperty("currentIndex").toInt();
	
	if (options.size() == 0)
		index = -1;
	else if (index >= int(options.size()))
		index = 0;
	
	std::string selected = "";
	if (index >= 0)
		selected = options[size_t(index)];
	
	return new OptionList(options, selected);
}

bool BoundQMLComboBox::isOptionValid(Option *option)
{
	return dynamic_cast<OptionList*>(option) != nullptr;
}

bool BoundQMLComboBox::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::stringValue;
}

void BoundQMLComboBox::setUp()
{
	QMLListView::setUp();
	
	_setCurrentValue(_currentIndex, true, false);
	setItemProperty("initialized", true);
}

void BoundQMLComboBox::modelChangedHandler()
{
	std::vector<std::string> options;
	const Terms& terms = _model->terms();
	bool found = false;
	int index = 0;
	for (const Term& term : terms)
	{
		QString val = term.asQString();		
		options.push_back(val.toStdString());		
		if (val == _currentText)
		{
			found = true;
			break;
		}
		index++;
	}
	
	if (!found)
	{
		index = -1;
		if (terms.size() > 0U)
			index = 0;
	}
	
	if (_boundTo)
		_boundTo->resetOptions(options, index);
	
	_setCurrentValue(index, true, false);
	
	_resetItemWidth();
}

void BoundQMLComboBox::comboBoxChangeValueSlot(int index)
{
	const Terms& terms = _model->terms();
	if (index < 0 || index >= int(terms.size()))
		return;
	
	if (_currentIndex != index)
		_setCurrentValue(index);
}

void BoundQMLComboBox::_resetItemWidth()
{
	if (!_item)
		return;

	const Terms& terms = _model->terms();
	QMetaObject::invokeMethod(_item, "resetWidth", Q_ARG(QVariant, QVariant(terms.asQList())));
}

void BoundQMLComboBox::_setCurrentValue(int index, bool setComboBoxIndex, bool setOption)
{
	_currentIndex = index;
	_currentText.clear();
	_currentColumnType.clear();
	if (_currentIndex >= 0)
	{
		int rowCount = _model->rowCount();
		if (_currentIndex >= rowCount)
		{
			if (rowCount > 0)
				_currentIndex = -1;
			else
				_currentIndex = 0;
		}
		if (_currentIndex >= 0)
		{
			QModelIndex index(_model->index(_currentIndex, 0));
			_currentText = _model->data(index, ListModel::NameRole).toString();	
			_currentColumnType = _model->data(index, ListModel::ColumnTypeRole).toString();			
		}
	}
	setItemProperty("currentText", _currentText);
	setItemProperty("currentColumnType", _currentColumnType);
	if (setComboBoxIndex)
		setItemProperty("currentIndex", _currentIndex);
	if (setOption && _boundTo)
		_boundTo->set(size_t(_currentIndex));
}
