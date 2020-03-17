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
#include "../analysis/jaspcontrolbase.h"
#include <QAbstractListModel>


BoundQMLComboBox::BoundQMLComboBox(JASPControlBase* item)
	: JASPControlWrapper(item)
	, QMLListView(item)
	, BoundQMLItem()
{
	_currentIndex = getItemProperty("currentIndex").toInt();	
	_model = new ListModelTermsAvailable(this);

	connect(_model, &ListModelTermsAvailable::allAvailableTermsChanged, this, &BoundQMLComboBox::modelChangedHandler);
	if (getItemProperty("addEmptyValue").toBool())
		_model->addEmptyValue();

	readModelProperty(&_labelToValueMap);
	QMapIterator<QString, QString> it(_labelToValueMap);
	while (it.hasNext())
	{
		it.next();
		_valueToLabelMap[it.value()] = it.key();
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
		QList<QString> labels = _model->terms().asQList();
		if (labels.size() > 0)
		{
			if (selectedValue.isEmpty())
				index = 0;
			else
			{
				QString selectedLabel = _valueToLabelMap.contains(selectedValue) ? _valueToLabelMap[selectedValue] : selectedValue;
				index = labels.indexOf(selectedLabel);
				if (index == -1)
				{
					addControlError(tr("Unknown option %1 in DropDown %2").arg(selectedValue).arg(name()));
					index = 0;
				}
			}
		}
		std::vector<std::string> options = _getOptionValues();
		_boundTo->resetOptions(options, index);
		
		_setCurrentValue(index, true, false);
		
		_resetItemWidth();
		setItemProperty("initialized", true);
	}
	else
		addControlError(tr("Unknown error in ComboBox %1").arg(name()));
}

void BoundQMLComboBox::resetQMLItem(JASPControlBase *item)
{
	BoundQMLItem::resetQMLItem(item);
	
	setItemProperty("model", QVariant::fromValue(_model));
	setItemProperty("currentIndex", _currentIndex);
	setItemProperty("currentText", _currentText);
	setItemProperty("currentValue",	tq(_boundTo->value()));
	setItemProperty("currentColumnType", _currentColumnType);
	_resetItemWidth();

	if (_item)
		QQuickItem::connect(_item, SIGNAL(activated(int)), this, SLOT(comboBoxChangeValueSlot(int)));
}

std::vector<std::string> BoundQMLComboBox::_getOptionValues()
{
	std::vector<std::string> values;
	const Terms& terms = _model->terms();
	for (const Term& term : terms)
	{
		QString label = term.asQString();
		QString value = _labelToValueMap.contains(label) ? _labelToValueMap[label] : label;
		values.push_back(value.toStdString());
	}
	
	return values;
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

	if (form())
		connect(form(), &AnalysisForm::languageChanged, this, &BoundQMLComboBox::resetValues);

	if (hasSource())
	{
		_model->resetTermsFromSourceModels(false);
		_resetItemWidth();
	}
	
	_setCurrentValue(_currentIndex, true, false);
}

void BoundQMLComboBox::resetValues()
{
	_valueToLabelMap.clear();
	_labelToValueMap.clear();

	readModelProperty(&_labelToValueMap);
	QMapIterator<QString, QString> it(_labelToValueMap);
	while (it.hasNext())
	{
		it.next();
		_valueToLabelMap[it.value()] = it.key();
	}

	// In case of retranslation, QML sets back its original model, so we have to set it again to our own model.
	setItemProperty("model", QVariant::fromValue(_model));

	_resetItemWidth();

	_setCurrentValue(_currentIndex, true, false);
}

void BoundQMLComboBox::modelChangedHandler()
{
	std::vector<std::string> options;
	const Terms& terms = _model->terms();
	int index = 0;
	int currentIndex = -1;
	for (const Term& term : terms)
	{
		QString label = term.asQString();
		options.push_back(label.toStdString());
		if (label == _currentText)
			currentIndex = index;
		index++;
	}
	
	if (currentIndex == -1)
	{
		if (int(terms.size()) > _currentIndex)
			currentIndex = _currentIndex;
		else if (terms.size() > 0U)
			currentIndex = 0;
	}
	
	if (_boundTo)
		_boundTo->resetOptions(options, currentIndex);
	
	_setCurrentValue(currentIndex, true, true);
	
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
	// Cannot use _boundTo to get the current value, because when _boundTo is changed (by setting the current index),
	// it emits a signal that can be received by a slot that needs already the currentValue.
	// This is in particular needed in CustomContrast
	setItemProperty("currentValue", _labelToValueMap.contains(_currentText) ? _labelToValueMap[_currentText] : _currentText);
	setItemProperty("currentColumnType", _currentColumnType);

	if (setComboBoxIndex)
		setItemProperty("currentIndex", _currentIndex);

	if (_boundTo && setOption)
		_boundTo->set(size_t(_currentIndex));
}
