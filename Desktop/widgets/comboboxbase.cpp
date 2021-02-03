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
	: JASPListControl(parent), BoundControlBase(this)
{
	_controlType = ControlType::ComboBox;
}

void ComboBoxBase::bindTo(const Json::Value& value)
{
	std::vector<std::string> values = _model->getValues();
	std::string selectedValue = value.asString();
	int index = -1;

	if (values.size() > 0)
	{
		if (selectedValue.empty())	index = 0;
		else
		{
			auto itr = std::find(values.begin(), values.end(), selectedValue);
			if (itr == values.end())
			{
				addControlError(tr("Unknown option %1 in DropDown %2").arg(tq(selectedValue)).arg(name()));
				index = 0;
			}
			index = int(std::distance(values.begin(), itr));
		}
	}

	_setCurrentProperties(index, false);

	_resetItemWidth();

	BoundControlBase::bindTo(value);
}

int ComboBoxBase::_getStartIndex()
{
	if (!startValue().isEmpty())	return _model->getIndexOfValue(startValue());
	if (currentIndex() != -1)		return currentIndex();
	if (!currentValue().isEmpty())	return _model->getIndexOfValue(currentValue());
	if (!currentText().isEmpty())	return _model->getIndexOfLabel(currentText());
	return -1;
}

Json::Value ComboBoxBase::createJson()
{
	std::vector<std::string> options = _model->getValues();
	
	int index = _getStartIndex();

	if (options.size() == 0)								index = -1;
	else if (index == -1 || (index >= int(options.size())))	index = 0;
	
	std::string selected = index >= 0 ? options[size_t(index)] : "";
	
	return selected;
}

bool ComboBoxBase::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::stringValue;
}

void ComboBoxBase::setUp()
{
	JASPListControl::setUp();

	_model->resetTermsFromSources();

	connect(this,	&ComboBoxBase::activated,					this,	&ComboBoxBase::activatedSlot);
	connect(this,	&JASPListControl::addEmptyValueChanged,		[this] () { _model->resetTermsFromSources(); }	);
	connect(this,	&ComboBoxBase::currentIndexChanged,			[this] () { _setCurrentProperties(currentIndex()); } ); // Case when currentIndex is changed in QML

	if (form())
		connect(form(), &AnalysisForm::languageChanged,			[this] () { _model->resetTermsFromSources(); }	);

}

void ComboBoxBase::setUpModel()
{
	_model = new ListModelLabelValueTerms(this);
	JASPListControl::setUpModel();
}

void ComboBoxBase::termsChangedHandler()
{
	std::vector<std::string> values = _model->getValues();
	int index = -1;

	if (values.size() > 0)
	{
		if (initialized())
		{
			auto itr = std::find(values.begin(), values.end(), fq(_currentValue));

			if (itr == values.end())	index = _getStartIndex();
			else						index = int(std::distance(values.begin(), itr));
		}
		else							index = _getStartIndex();

		if (index < 0 || index > int(values.size())) index = 0;
	}

	_setCurrentProperties(index);
	
	_resetItemWidth();
}

void ComboBoxBase::activatedSlot(int index)
{
	_setCurrentProperties(index);
}

void ComboBoxBase::_resetItemWidth()
{
	const Terms& terms = _model->terms();
	QMetaObject::invokeMethod(this, "resetWidth", Q_ARG(QVariant, QVariant(terms.asQList())));
}

void ComboBoxBase::setCurrentText(QString text)
{
	if (initialized())
		_setCurrentProperties(_model->getIndexOfLabel(text));
	else
		_currentText = text;
}

void ComboBoxBase::setCurrentValue(QString value)
{
	if (initialized())
		_setCurrentProperties(_model->getIndexOfValue(value));
	else
		_currentValue = value;
}
void ComboBoxBase::setCurrentIndex(int index)
{
	if (initialized())
		_setCurrentProperties(index);
	else
		_currentIndex = index; // In this case it is used as start index
}

void ComboBoxBase::_setCurrentProperties(int index, bool bindValue)
{
	QString currentColumnType, currentValue, currentText, currentColumnTypeIcon;

	if (index >= _model->rowCount())	index = 0;

	if (index >= 0)
	{
		QModelIndex modelIndex(_model->index(index, 0));
		currentColumnType = _model->data(modelIndex, ListModel::ColumnTypeRole).toString();
		currentColumnTypeIcon = _model->data(modelIndex, ListModel::ColumnTypeIconRole).toString();
		currentText = _model->data(modelIndex, ListModel::NameRole).toString();
		currentValue = _model->data(modelIndex, ListModel::ValueRole).toString();
	}

	// emit signals when all values are set, so that when 1 of the signals is caught,
	// all values are coherent
	bool emitCurrentTextSignal				= (_currentText != currentText);
	bool emitCurrentValueSignal				= (_currentValue != currentValue);
	bool emitCurrentColumnTypeSignal		= (_currentColumnType != currentColumnType);
	bool emitCurrentColumnTypeIconSignal	= (_currentColumnTypeIcon != currentColumnTypeIcon);
	bool emitCurrentIndexSignal				= (_currentIndex != index);

	_currentText							= currentText;
	_currentValue							= currentValue;
	_currentColumnType						= currentColumnType;
	_currentColumnTypeIcon					= currentColumnTypeIcon;
	_currentIndex							= index;

	if (emitCurrentTextSignal)				emit currentTextChanged();
	if (emitCurrentValueSignal)				emit currentValueChanged();
	if (emitCurrentColumnTypeSignal)		emit currentColumnTypeChanged();
	if (emitCurrentColumnTypeIconSignal)	emit currentColumnTypeIconChanged();
	if (emitCurrentIndexSignal)				emit currentIndexChanged();

	if (bindValue)	setBoundValue(fq(_currentValue));
}
