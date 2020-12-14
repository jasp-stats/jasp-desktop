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
		
		_setCurrentProperties(index, false);
		
		_resetItemWidth();
	}
	else
		addControlError(tr("Unknown error in ComboBox %1").arg(name()));
}

int ComboBoxBase::_getStartIndex()
{
	if (!startValue().isEmpty())	return _model->getIndexOfValue(startValue());
	if (currentIndex() != -1)		return currentIndex();
	if (!currentValue().isEmpty())	return _model->getIndexOfValue(currentValue());
	if (!currentText().isEmpty())	return _model->getIndexOfLabel(currentText());
	return -1;
}

Option *ComboBoxBase::createOption()
{
	std::vector<std::string> options = _model->getValues();
	
	int index = _getStartIndex();

	if (options.size() == 0)								index = -1;
	else if (index == -1 || (index >= int(options.size())))	index = 0;
	
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

void ComboBoxBase::setUp()
{
	JASPListControl::setUp();

	_model->resetTermsFromSources();

	connect(this,	&JASPListControl::addEmptyValueChanged,		[this] () { _model->resetTermsFromSources(); }	);
	connect(this,	&ComboBoxBase::currentIndexChanged,			[this] () { _setCurrentProperties(currentIndex()); } ); // Case when currentIndex is changed in QML
	connect(this,	SIGNAL(activated(int)),						this,		SLOT(activatedSlot(int)));
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
	std::vector<std::string> options;
	const Terms& terms = _model->terms();
	int index = -1;

	if (initialized())
	{
		int counter = 0;
		for (const Term& term : terms)
		{
			QString label = term.asQString();
			QString value = _model->getValue(label);
			options.push_back(value.toStdString());
			if (value == _currentValue)
				index = counter;
			counter++;
		}

		if (index == -1) index = _getStartIndex();
	}
	else
		index = _getStartIndex();
	
	if (terms.size() == 0)									index = -1;
	else if (index == -1 || (index >= int(terms.size())))	index = 0;
	
	if (_boundTo)
		_boundTo->resetOptions(options, index);
	
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

void ComboBoxBase::_setCurrentProperties(int index, bool setOption)
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

	if (_boundTo && setOption)
		_boundTo->set(size_t(index));
}
