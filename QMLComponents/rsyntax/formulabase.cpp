//
// Copyright (C) 2013-2021 University of Amsterdam
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

#include "formulabase.h"
#include "formulasource.h"
#include "rsyntax.h"
#include "controls/sourceitem.h"
#include "analysisform.h"
#include "log.h"
#include "formulaparser.h"

FormulaBase::FormulaBase(QQuickItem *parent) : QQuickItem(parent)
{
	setVisible(false);
}

void FormulaBase::setUp()
{
	_leftFormulaSources  = FormulaSource::makeFormulaSources(this, _lhs);
	_rightFormulaSources = FormulaSource::makeFormulaSources(this, _rhs);

	connect(this, &FormulaBase::lhsChanged, this, [this]() { _leftFormulaSources  = FormulaSource::makeFormulaSources(this, _lhs); emit somethingChanged();} );
	connect(this, &FormulaBase::rhsChanged, this, [this]() { _rightFormulaSources = FormulaSource::makeFormulaSources(this, _rhs); emit somethingChanged();} );
}

// Generate the R Formula
QString FormulaBase::toString() const
{
	if (!_rSyntax)
		return "";

	QString result = _rSyntax->FunctionOptionIndent + _name + " = ";
	bool isEmpty = true;

	for (FormulaSource* formulaSource : _leftFormulaSources + _rightFormulaSources)
		if (isEmpty) isEmpty = formulaSource->isEmpty();

	if (isEmpty)
	{
		result += "NULL";
		return result;
	}

	auto addFormulaTerms = [] (const QVector<FormulaSource*>& formulaSources, bool isLhs) -> QString
	{
		QString result;

		bool first = true;
		for (FormulaSource* formulaSource: formulaSources)
		{
			if (formulaSource->isEmpty()) continue;

			QString terms = formulaSource->toString();
			if (terms.isEmpty()) continue;

			if (!first) result += " + ";
			first = false;

			result += terms;
		}

		if (isLhs && result.contains(" +")) result = "cbind(" + result.replace(" +", ",") + ")";
		return result;
	};

	result += addFormulaTerms(_leftFormulaSources, true) + " ~ " +  addFormulaTerms(_rightFormulaSources, false);

	// In case of a formula uses a VarialesList which has also extra controls for each variable (typically the isNuisance checkbox in Bayesian ANOVA),
	// then this information cannot be added in the formula. For this we need to add extra option with the values of these extra controls.
	for (FormulaSource* formulaSource : _leftFormulaSources + _rightFormulaSources)
	{
		QMap<QString, QString> extraOptions = formulaSource->additionalOptionStrings();
		for (const QString& key : extraOptions.keys())
		{
			if (!result.isEmpty())	
				result += ",\n";

			result += _rSyntax->FunctionOptionIndent + _rSyntax->getRSyntaxFromControlName(key) + " = " + extraOptions[key];
		}
	}

	// When terms come from different sources. only one can be unspecified, for the other one the user must specify in the script
	// which terms are in these sources.
	for (const QString& optionToSpecify : _getSourceList(_userMustSpecify))
	{
		ListModel* model = getModel(optionToSpecify);
		if (!model) 
			continue;

		QStringList elements = model->terms().asQList();

		if (elements.isEmpty()) 
			continue;

		if (!result.isEmpty())	
			result += ",\n";

		result += _rSyntax->FunctionOptionIndent + _rSyntax->getRSyntaxFromControlName(optionToSpecify) + " = ";

		if (elements.length() == 0)			result += "\"\"";
		else if (elements.length() == 1)	result += "\"" + elements[0] + "\"";
		else
		{
			result += "list(";
			bool first = true;
			for (const QString& element : elements)
			{
				if (!first) 
					result += ", ";

				first = false;
				result += "\"" + element + "\"";
			}
			result += ")";
		}
	}

	return result;
}

bool FormulaBase::parseRSyntaxOptions(Json::Value &options) const
{
	const Json::Value& formulaJson = options[fq(_name)];

	Log::log() << "Formula: " << formulaJson.toStyledString() << std::endl;
	if (formulaJson.empty()) return true;

	if (!formulaJson.isObject())
	{
		Log::log() << "Formula option is not an object!" << std::endl;
		return false;
	}

	const Json::Value	lhs = formulaJson["lhs"],
						rhs = formulaJson["rhs"];

	FormulaParser::ParsedTerms leftParsedTerms, rightParsedTerms;
	QString error;

	if (!FormulaParser::parse(lhs, true, leftParsedTerms, error) || !FormulaParser::parse(rhs, false, rightParsedTerms, error))
	{
		_rSyntax->addError(error);
		return false;
	}

	return _parseFormulaSources(_leftFormulaSources, leftParsedTerms, options) && _parseFormulaSources(_rightFormulaSources, rightParsedTerms, options);
}

AnalysisForm *FormulaBase::form() const
{
	return _rSyntax ? _rSyntax->form() : nullptr;
}

QStringList FormulaBase::modelSources() const
{
	QStringList result = sourcesThatMustBeSpecified();

	for (FormulaSource* formulaSource : _leftFormulaSources + _rightFormulaSources)
		result += formulaSource->modelSources();

	return result;
}

QStringList FormulaBase::sourcesThatMustBeSpecified() const
{
	return _getSourceList(_userMustSpecify);
}

QStringList FormulaBase::extraOptions(bool useOptionName, bool onlyFormula) const
{
	QStringList result;

	for (FormulaSource* formulaSource : _leftFormulaSources + _rightFormulaSources)
		result += formulaSource->extraOptions(useOptionName, onlyFormula);

	return result;
}

QStringList FormulaBase::_getSourceList(const QVariant &var) const
{
	QStringList result;
	for (const QVariant& modelSpec : SourceItem::getListVariant(var))
	{
		if (modelSpec.canConvert<QString>())
		{
			QString sourceName = modelSpec.toString();
			if (!sourceName.isEmpty())	
				result.push_back(sourceName);
		}
		else if (modelSpec.canConvert<QMap<QString, QVariant> >())
		{
			QMap<QString, QVariant> map = modelSpec.toMap();
			if (map.contains("name"))
			{
				QString sourceName = map["name"].toString();
				if (!sourceName.isEmpty()) 
					result.push_back(sourceName);
			}
		}
	}
	return result;
}


bool FormulaBase::_parseFormulaSources(const QVector<FormulaSource*>& formulaSources, FormulaParser::ParsedTerms& parsedTerms, Json::Value& options) const
{
	if (parsedTerms.fixedTerms.terms().empty())
		return true;

	for (const FormulaSource* formulaSource : formulaSources)
		parsedTerms = formulaSource->fillOptionsWithParsedTerms(parsedTerms, options);

	if (!parsedTerms.fixedTerms.terms().empty() && !_rSyntax->hasError())
		_rSyntax->addError(tr("Could not find term %1 in Formula").arg(parsedTerms.fixedTerms.terms()[0].asQString()));

	return !_rSyntax->hasError();
}

ListModel *FormulaBase::getModel(const QString &name) const
{
	AnalysisForm	* aform 		= form();
	ListModel		* model 		= nullptr;
	JASPListControl	* listControl 	= nullptr;

	if (!aform)	
		return model;

	JASPControl* control = aform->getControl(name);
	if (!control) 
		aform->addFormError(tr("Source %1 in Formula not found").arg(name));
	else
	{
		listControl = qobject_cast<JASPListControl*>(control);
		if (!listControl) aform->addFormError(tr("Source %1 in Formula is not a Variables List").arg(name));
		else model = listControl->model();
	}

	return model;
}

void FormulaBase::componentComplete()
{
	AnalysisForm* form = qobject_cast<AnalysisForm*>(parent());
	
	if (form)
		_rSyntax = form->rSyntax();

	if (_rSyntax)
		_rSyntax->addFormula(this);
}
