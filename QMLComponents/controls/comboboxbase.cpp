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
#include "analysisform.h"
#include "log.h"
#include "jasptheme.h"

ComboBoxBase::ComboBoxBase(QQuickItem* parent)
	: JASPListControl(parent), BoundControlBase(this)
{
	_controlType = ControlType::ComboBox;
	_hasUserInteractiveValue = true;
}

void ComboBoxBase::bindTo(const Json::Value& value)
{
	_model->resetTermsFromSources();

	Json::Value valuePart = _isValueWithTypes(value) ? value["value"] : value;
	std::string selectedValue = valuePart.asString();

	std::vector<std::string> values = _model->getValues();
	int index = -1;

	if (values.size() > 0)
	{
		if (selectedValue.empty())	index = 0;
		else
		{
			auto itr = std::find(values.begin(), values.end(), selectedValue);

			if (itr == values.end())
			{
				// Buggy situation: the value is not one of the available values of the DropDown.
				// This might happen with a corrupted JASP file, or an old bug like https://github.com/jasp-stats/jasp-test-release/issues/1836
				// Before throwing an error message, let's be a bit flexible: if we can find a value which is case-insensitive equal to the selectedValue,
				// then we can be confident that it is the right one.
				auto caseInsensitiveEquals = [&](const std::string& s)
				{
					return std::equal(s.begin(), s.end(),
									  selectedValue.begin(), selectedValue.end(),
									  [](char a, char b) { return tolower(a) == tolower(b); });
				};
				itr = std::find_if(values.begin(), values.end(), caseInsensitiveEquals);
				if (itr != values.end())
					Log::log() << "Option " << selectedValue << " in DropDown " << name() << " found but not with the same case: " << *itr << std::endl;
			}

			if (itr == values.end())
			{
				// Try also to find a label equals to the selectedValue.
				auto labelEqualts = [&](const std::string& s)
				{
					return fq(_model->getLabel(tq(s))) == selectedValue;
				};
				itr = std::find_if(values.begin(), values.end(), labelEqualts);
				if (itr != values.end())
					Log::log() << "Option " << selectedValue << " in DropDown " << name() << " found but as label." << std::endl;
			}

			if (itr == values.end())
			{
				addControlError(tr("Unknown option %1 in DropDown %2").arg(tq(selectedValue)).arg(name()));
				index = 0;
				// Maybe the values will be reset afterwards due to some QML/JavaScript dependencies: use this selectedValue if the model is reset during the initialization of the form
				_unusedInitialValue = selectedValue;
			}
			index = int(std::distance(values.begin(), itr));
		}
	}
	else if (!selectedValue.empty())
		// The control is bound with a value, but its model is empty.
		// Probably the values are set with a direct reference of a property of another control, like varList.levels, and this control is not yet initialized.
		// (as the combobox has no direct reference to the varList self, it cannot add a dependency in _depends).
		// So keep this value, and use it if the model is reset during the initialization of the form.
		_unusedInitialValue = selectedValue;

	_setCurrentProperties(index);

	_resetItemWidth();

	if (_control->encodeValue())
	{
		Json::Value newValue(Json::objectValue);
		Json::Value type = _isValueWithTypes(value) ? value["types"] : Json::nullValue;
		std::string currentValue = fq(_currentValue);
		newValue["value"] = currentValue;
		newValue["types"] = (currentValue != selectedValue || type.isNull()) ? _findType(currentValue) : type;
		BoundControlBase::bindTo(newValue);
	}
	else
		BoundControlBase::bindTo(fq(_currentValue));
}

int ComboBoxBase::_getStartIndex() const
{
	if (!startValue().isEmpty())	return _model->getIndexOfValue(startValue());
	if (currentIndex() != -1)		return currentIndex();
	if (!currentValue().isEmpty())	return _model->getIndexOfValue(currentValue());
	if (!currentText().isEmpty())	return _model->getIndexOfLabel(currentText());
	return -1;
}

Json::Value ComboBoxBase::createJson() const
{
	std::vector<std::string> options = _model->getValues();
	
	int index = _getStartIndex();

	if (options.size() == 0)								index = -1;
	else if (index == -1 || (index >= int(options.size())))	index = 0;
	
	std::string selected = index >= 0 ? options[size_t(index)] : "";
	
	if (_control->encodeValue())
	{
		Json::Value json(Json::objectValue);
		json["value"] = selected;
		json["types"] = _findType(selected);
		return json;
	}
	else
		return selected;
}

bool ComboBoxBase::isJsonValid(const Json::Value &optionValue) const
{
	return optionValue.type() == Json::stringValue || optionValue.type() == Json::objectValue;
}

void ComboBoxBase::setUp()
{
	JASPListControl::setUp();

	_model->resetTermsFromSources();

	connect(this,	&ComboBoxBase::activated,					this,	&ComboBoxBase::activatedSlot);
	connect(this,	&JASPListControl::addEmptyValueChanged,		[this] () { _model->resetTermsFromSources(); }	);
	connect(this,	&ComboBoxBase::currentIndexChanged,			[this] () { _setCurrentProperties(currentIndex()); } ); // Case when currentIndex is changed in QML
	connect(this,	&ComboBoxBase::currentValueChanged,			[this] () { if (containsVariables()) checkLevelsConstraints(); } );

	if (form())
	{
		connect(form(), &AnalysisForm::languageChanged,			[this] () { _model->resetTermsFromSources(); }	);
		connect(form(), &AnalysisForm::analysisChanged,			[this] () { _unusedInitialValue = ""; });
	}
}

void ComboBoxBase::setUpModel()
{
	_model = new ListModelLabelValueTerms(this);
	JASPListControl::setUpModel();
}

std::vector<std::string> ComboBoxBase::usedVariables() const
{
	if (containsVariables())	return { fq(_currentValue) };
	else						return {};
}

void ComboBoxBase::termsChangedHandler()
{
	JASPListControl::termsChangedHandler();

	std::vector<std::string> values = _model->getValues();
	int index = -1;

	if (values.size() > 0)
	{
		if (initialized())
		{
			auto itr = std::find(values.begin(), values.end(), fq(_currentValue));

			if (!_unusedInitialValue.empty())
			{
				auto lostValueItr = std::find(values.begin(), values.end(), _unusedInitialValue);
				if (lostValueItr != values.end())
				{
					itr = lostValueItr;
					_orgValue = _unusedInitialValue;
					_unusedInitialValue = "";
					clearControlError();
				}
			}

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

bool ComboBoxBase::_checkLevelsConstraints()
{
	return _checkLevelsConstraintsForVariable(_currentValue);
}

void ComboBoxBase::_resetItemWidth()
{
	double maxWidth = 0;
	QString longestValue;

	QFontMetricsF& metrics = JaspTheme::fontMetrics();

	if (_addEmptyValue)
	{
		maxWidth = metrics.horizontalAdvance(_placeHolderText);
		longestValue = _placeHolderText;
	}
	for (const Term& term : model()->terms())
	{
		double termWidth = metrics.horizontalAdvance(term.asQString());
		if (maxWidth < termWidth)
		{
			maxWidth = termWidth;
			longestValue = term.asQString();
		}
	}

	if (_longestValue != longestValue)
	{
		_longestValue = longestValue;
		emit longestValueChanged();
	}
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
	QString currentColumnType, currentColumnRealType, currentValue, currentText, currentColumnTypeIcon;

	if (index >= _model->rowCount())	
		index = 0;

	if (index >= 0)
	{
		QModelIndex modelIndex(_model->index(index, 0));
		
		currentColumnType		= _model->data(modelIndex, ListModel::ColumnTypeRole	).toString();
		currentColumnRealType	= _model->data(modelIndex, ListModel::ColumnRealTypeRole).toString();
		currentColumnTypeIcon	= _model->data(modelIndex, ListModel::ColumnTypeIconRole).toString();
		currentText				= _model->data(modelIndex, ListModel::NameRole			).toString();
		currentValue			= _model->data(modelIndex, ListModel::ValueRole			).toString();
	}

	// emit signals when all values are set, so that when 1 of the signals is caught,
	// all values are coherent
	bool	emitCurrentTextSignal				= _currentText				!= currentText,
			emitCurrentValueSignal				= _currentValue				!= currentValue,
			emitCurrentIndexSignal				= _currentIndex				!= index,
			emitCurrentColumnTypeSignal			= _currentColumnType		!= currentColumnType,
			emitCurrentColumnRealTypeSignal		= _currentColumnRealType	!= currentColumnRealType,
			emitCurrentColumnTypeIconSignal		= _currentColumnTypeIcon	!= currentColumnTypeIcon;

			_currentText						= currentText;
			_currentValue						= currentValue;
			_currentIndex						= index;
			_currentColumnType					= currentColumnType;
			_currentColumnRealType				= currentColumnRealType;
			_currentColumnTypeIcon				= currentColumnTypeIcon;

	if (emitCurrentTextSignal)				emit currentTextChanged();
	if (emitCurrentValueSignal)				emit currentValueChanged();
	if (emitCurrentColumnTypeSignal)		emit currentColumnTypeChanged();
	if (emitCurrentColumnTypeIconSignal)	emit currentColumnTypeIconChanged();
	if (emitCurrentIndexSignal)				emit currentIndexChanged();

	if (bindValue && initialized())	
		setBoundValue(fq(_currentValue));
	
	if(emitCurrentValueSignal && containsVariables())
		emit usedVariablesChanged();
}


QString	ComboBoxBase::helpMD(int depth) const
{
	QStringList markdown;

	printLabelMD(markdown, depth);
	markdown << info();

	// If one of the option has an info property, then display the options as an unordered list
	if (_hasOptionInfo())
	{
		for (const Term& term : _model->terms())
		{
			QString label = term.asQString(),
					info = _model->getInfo(label);
			markdown << "\n" << QString{depth * 2, ' '} << "- *" << label << "*";
			if (!info.isEmpty())
				markdown << (": " + info);
		}
	}
	else
	{
		markdown << "\n" << QString{depth * 2, ' '};
		// Display the options in one line separated by a comma.
		markdown << model()->terms().asQList().join(", ");
	}


	return markdown.join("") + "\n";
}

bool ComboBoxBase::_hasOptionInfo() const
{
	for (const Term& term : _model->terms())
	{
		if (!_model->getInfo(term.asQString()).isEmpty())
			return true;
	}

	return false;
}

bool ComboBoxBase::hasInfo() const
{
	return JASPControl::hasInfo() || _hasOptionInfo();
}

std::string ComboBoxBase::_findType(std::string value) const
{
	// Find the type of this value in the terms of the model
	columnType type = columnType::unknown;
	if (!value.empty())
	{
		const Terms& terms = model()->terms();
		int index = terms.indexOf(value);
		if (index >= 0)
			type = terms.at(index).type();
	}
	return columnTypeToString(type);
}

void ComboBoxBase::setBoundValue(const Json::Value &value, bool emitChanges)
{
	if (_control->encodeValue())
	{
		Json::Value newValue;

		if (_isValueWithTypes(value))
			newValue = value;
		else
			newValue["value"] = value;

		newValue["types"] = _findType(newValue["value"].asString());
		BoundControlBase::setBoundValue(newValue, emitChanges);
	}
	else
		BoundControlBase::setBoundValue(value, emitChanges);
}
