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

#include "comboboxbase.h"
#include "../analysis/analysisform.h"


ComboBoxBase::ComboBoxBase(QQuickItem* parent)
	: JASPListControl(parent)
{
	_controlType = ControlType::ComboBox;
}

void ComboBoxBase::bindTo(Option *option)
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
				QString selectedLabel = _model->getLabel(selectedValue);
				index = labels.indexOf(selectedLabel);
				if (index == -1)
				{
					addControlError(tr("Unknown option %1 in DropDown %2").arg(selectedValue).arg(name()));
					index = 0;
				}
			}
		}
		std::vector<std::string> options = _model->getValues();
		_boundTo->resetOptions(options, index);
		
		_setCurrentValue(index, false);
		
		_resetItemWidth();
		setProperty("initialized", true);
	}
	else
		addControlError(tr("Unknown error in ComboBox %1").arg(name()));
}

Option *ComboBoxBase::createOption()
{
	std::vector<std::string> options = _model->getValues();
	
	int index = startValue().isEmpty() ? indexDefaultValue() : _model->getIndexOfValue(startValue());

	if (options.size() == 0)
		index = -1;
	else if (index >= int(options.size()))
		index = 0;
	
	std::string selected = "";
	if (index >= 0)
		selected = options[size_t(index)];
	
	return new OptionList(options, selected);
}

bool ComboBoxBase::isOptionValid(Option *option)
{
	return dynamic_cast<OptionList*>(option) != nullptr;
}

bool ComboBoxBase::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::stringValue;
}

void ComboBoxBase::setLabelValues()
{
	_model->setLabelValuesFromSource();
	_model->refresh();

	_resetItemWidth();
}

void ComboBoxBase::setUp()
{
	JASPListControl::setUp();

	connect(_model, &ListModelTermsAvailable::allAvailableTermsChanged, this, &ComboBoxBase::termsChangedHandler);
	connect(this,	&JASPListControl::addEmptyValueChanged,				this, &ComboBoxBase::termsChangedHandler);
	connect(this, SIGNAL(activated(int)), this, SLOT(comboBoxChangeValueSlot(int)));
	if (form())
		connect(form(), &AnalysisForm::languageChanged, this, &ComboBoxBase::languageChangedHandler);

	setLabelValues();
}

void ComboBoxBase::setUpModel()
{
	_model = new ListModelLabelValueTerms(this);
	JASPListControl::setUpModel();
}

void ComboBoxBase::languageChangedHandler()
{
	setupSources();

	setLabelValues();
}

void ComboBoxBase::termsChangedHandler()
{
	setLabelValues();
	_resetOptions();
}

void ComboBoxBase::_resetOptions()
{
	std::vector<std::string> options;
	const Terms& terms = _model->terms();
	int index = 0;
	int currentIndex = -1;
	for (const Term& term : terms)
	{
		QString label = term.asQString();
		QString value = _model->getValue(label);
		options.push_back(value.toStdString());
		if (value == _currentValue)
			currentIndex = index;
		index++;
	}
	
	if (currentIndex == -1)
		currentIndex = startValue().isEmpty() ? indexDefaultValue() : _model->getIndexOfValue(startValue());
	
	if (_boundTo)
		_boundTo->resetOptions(options, currentIndex);
	
	_setCurrentValue(currentIndex, true);
	
	_resetItemWidth();
}

void ComboBoxBase::comboBoxChangeValueSlot(int index)
{
	if (index < 0 || index >= _model->rowCount())
		return;
	
	_setCurrentValue(index);
}

void ComboBoxBase::_resetItemWidth()
{
	const Terms& terms = _model->terms();
	QMetaObject::invokeMethod(this, "resetWidth", Q_ARG(QVariant, QVariant(terms.asQList())));
}

void ComboBoxBase::_setCurrentValue(int index, bool setOption)
{
	QString currentColumnType, currentValue, currentText;

	if (index >= 0)
	{
		int rowCount = _model->rowCount();
		if (index >= rowCount)
		{
			if (rowCount > 0)
				index = -1;
			else
				index = 0;
		}
		if (index >= 0)
		{
			QModelIndex modelIndex(_model->index(index, 0));
			currentColumnType = _model->data(modelIndex, ListModel::ColumnTypeRole).toString();
			currentText = _model->data(modelIndex, ListModel::NameRole).toString();
			currentValue = _model->data(modelIndex, ListModel::ValueRole).toString();
		}
	}

	// Cannot use _boundTo to get the current value, because when _boundTo is changed (by setting the current index),
	// it emits a signal that can be received by a slot that needs already the currentValue.
	// This is in particular needed in CustomContrast
	setCurrentText(currentText);
	setCurrentValue(currentValue);
	setCurrentColumnType(currentColumnType);

	int currentIndex = property("currentIndex").toInt();
	if (index != currentIndex)
		setProperty("currentIndex", index);
	setProperty("test", index);

	if (_boundTo && setOption)
		_boundTo->set(size_t(index));
}
