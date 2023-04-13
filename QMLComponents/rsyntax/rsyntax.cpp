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

#include "rsyntax.h"
#include "appinfo.h"
#include "analysisform.h"
#include "log.h"
#include <QQmlContext>
#include "formulasource.h"
#include "controls/jasplistcontrol.h"
#include "boundcontrols/boundcontrolterms.h"

QString RSyntax::FunctionOptionIndent	= "          ";
QString RSyntax::FunctionLineIndent		= "   ";


RSyntax::RSyntax(AnalysisForm *form) : QObject(form), _form(form)
{
}

QVariantList RSyntax::controlNameToRSyntaxMap() const
{
	QVariantList result;

	for (const QString& key : _controlNameToRSyntaxMap.keys())
	{
		QVariantMap map;
		map[key] =  _controlNameToRSyntaxMap[key];
		result.push_back(map);
	}

	return result;
}

bool RSyntax::setControlNameToRSyntaxMap(const QVariantList &conversions)
{
	QMap<QString, QString> newMap;
	for (const QVariant& conversion : conversions)
	{
		if (conversion.canConvert<QVariantMap>())
		{
			QVariantMap map = conversion.toMap();
			if (map.size() > 0)
			{
				QString key  = map.firstKey();
				newMap[key] = map[key].toString();
			}
		}
	}

	if (newMap != _controlNameToRSyntaxMap)
	{
		_controlNameToRSyntaxMap = newMap;

		_rSyntaxToControlNameMap = QMap<QString, QString>();
		for (const QString& key : _controlNameToRSyntaxMap.keys())
			_rSyntaxToControlNameMap[_controlNameToRSyntaxMap[key]] = key;

		return true;
	}

	return false;
}

QString RSyntax::generateSyntax(bool showAllOptions, bool useHtml) const
{
	QString result;

	QString newLine = useHtml ? "<br>" : "\n",
			indent = useHtml ? "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" : FunctionOptionIndent;


	result = _analysisFullName() + "(" + newLine;
	if (showAllOptions)
		result += indent + "data = NULL," + newLine;
	result += indent + "version = \"" + _form->version() + "\"";

	QStringList formulaSources;
	for (FormulaBase* formula : _formulas)
	{
		bool isNull = false;
		result += "," + newLine + formula->toString(newLine, indent, isNull);
		if (!isNull)
			formulaSources.append(formula->modelSources());
	}

	const Json::Value& boundValues = _form->boundValues();

	for (const std::string& member : boundValues.getMemberNames())
	{
		QString memberQ = tq(member);
		if (member == ".meta" || formulaSources.contains(memberQ)) continue;

		JASPControl* control = _form->getControl(memberQ);
		BoundControl* boundControl = control ? control->boundControl() : nullptr;
		if (!boundControl)
		{
			Log::log() << "No control found with name " << memberQ << " in RSyntax" << std::endl;
			continue;
		}

		const Json::Value& defaultValue = boundControl->defaultBoundValue();
		const Json::Value& foundValue = boundValues.get(member, Json::Value::null);
		if (showAllOptions || (defaultValue != foundValue))
		{
			bool isDifferent = true;
			// Sometimes a double value is set as integer, so their json value is different
			// Check whether there are really different.
			if (!showAllOptions && defaultValue.isNumeric() && foundValue.isNumeric())
				isDifferent = !qFuzzyCompare(defaultValue.asDouble(), foundValue.asDouble());
			if (isDifferent)
			{
				result += "," + newLine + indent + getRSyntaxFromControlName(control) + " = ";

				JASPListControl* listControl = qobject_cast<JASPListControl*>(control);
				if (listControl && !listControl->hasRowComponent() && listControl->containsInteractions())
					result += _transformInteractionTerms(listControl->model());
				else
					result += transformJsonToR(foundValue);
			}
		}
	}

	result += ")";

	return result;
}

QString RSyntax::generateWrapper() const
{
	QString result = "\
#\n\
# Copyright (C) 2013-2022 University of Amsterdam\n\
#\n\
# This program is free software: you can redistribute it and/or modify\n\
# it under the terms of the GNU General Public License as published by\n\
# the Free Software Foundation, either version 2 of the License, or\n\
# (at your option) any later version.\n\
#\n\
# This program is distributed in the hope that it will be useful,\n\
# but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
# GNU General Public License for more details.\n\
#\n\
# You should have received a copy of the GNU General Public License\n\
# along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\
#\n\
\n\
# This is a generated file. Don't change it\n\
\n\
";
	result += _form->name() + " <- function(\n";
	result += FunctionOptionIndent + "data = NULL,\n";
	result += FunctionOptionIndent + "version = \"" + form()->version() + "\"";
	for (FormulaBase* formula : _formulas)
		result += ",\n" + FunctionOptionIndent + formula->name() + " = NULL";

	for (FormulaBase* formula : _formulas)
	{
		QStringList extraOptions = formula->extraOptions();
		for (const QString& extraOption : extraOptions)
			result += ",\n" + FunctionOptionIndent + extraOption + " = NULL";
	}

	const Json::Value& boundValues = _form->boundValues();
	QStringList optionsWithFormula;
	for (FormulaBase* formula : _formulas)
		optionsWithFormula += formula->extraOptions(true, true);

	for (const std::string& member : boundValues.getMemberNames())
	{
		QString memberQ = tq(member);
		if (member == ".meta") continue;

		JASPControl* control = _form->getControl(memberQ);
		BoundControl* boundControl = control ? control->boundControl() : nullptr;
		if (!boundControl)
		{
			Log::log() << "No control found with name " << memberQ << " in RSyntax" << std::endl;
			continue;
		}

		const Json::Value& defaultValue = boundControl->defaultBoundValue();
		result += ",\n" + FunctionOptionIndent + getRSyntaxFromControlName(control) + " = " + transformJsonToR(defaultValue);

		JASPListControl* listControl = qobject_cast<JASPListControl*>(control);
		if (listControl)
			optionsWithFormula.push_back(getRSyntaxFromControlName(control));
	}

	result += ") {\n\n"
	+ FunctionLineIndent + "defaultArgCalls <- formals(" + _analysisFullName() + ")\n"
	+ FunctionLineIndent + "defaultArgs <- lapply(defaultArgCalls, eval)\n"
	+ FunctionLineIndent + "options <- as.list(match.call())[-1L]\n"
	+ FunctionLineIndent + "options <- lapply(options, eval)\n"
	+ FunctionLineIndent + "defaults <- setdiff(names(defaultArgs), names(options))\n"
	+ FunctionLineIndent + "options[defaults] <- defaultArgs[defaults]\n"
	+ FunctionLineIndent + "options[[\"data\"]] <- NULL\n"
	+ FunctionLineIndent + "options[[\"version\"]] <- NULL\n\n";

	for (FormulaBase* formula : _formulas)
	{
		result += ""
		+ FunctionLineIndent + "if (!is.null(formula)) {\n"
		+ FunctionLineIndent + FunctionLineIndent + "if (!inherits(" + formula->name() + ", \"formula\")) {\n"
		+ FunctionLineIndent + FunctionLineIndent + FunctionLineIndent + formula->name() + " <- as.formula(" + formula->name() + ")\n"
		+ FunctionLineIndent + FunctionLineIndent + "}\n"
		+ FunctionLineIndent + FunctionLineIndent + "options$" + formula->name() + " <- jaspBase::jaspFormula(" + formula->name() + ", data)\n"
		+ FunctionLineIndent + "}\n\n";
	}

	if (optionsWithFormula.length() > 0)
	{
		result += ""
		+ FunctionLineIndent + "optionsWithFormula <- c(";
		bool isFirst = true;
		for (const QString& optionName : optionsWithFormula)
		{
			if (!isFirst) result += ", ";
			result += "\"" + optionName + "\"";
			isFirst = false;
		}
		result += ")\n"
		+ FunctionLineIndent + "for (name in optionsWithFormula) {\n"
		+ FunctionLineIndent + FunctionLineIndent + "if ((name %in% optionsWithFormula) && inherits(options[[name]], \"formula\")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)"
		+ FunctionLineIndent + "}\n"
		+ "\n";
	}

	result += ""
	+ FunctionLineIndent + "return(jaspBase::runWrappedAnalysis(\"" + _analysisFullName() + "\", data, options, version))\n"
	+ "}";

	return result;
}

void RSyntax::setUp()
{
	for (FormulaBase* formula : _formulas)
	{
		formula->setUp();
		connect(formula,	&FormulaBase::somethingChanged, this, &RSyntax::somethingChanged, Qt::QueuedConnection);
	}
}

FormulaBase* RSyntax::getFormula(const QString& name) const
{
	for (FormulaBase* formula : _formulas)
		if (formula->name() == name)
			return formula;

	return nullptr;
}

void RSyntax::addFormula(FormulaBase *formula)
{
	if (getFormula(formula->name()))
		_form->addFormError(tr("Formula with name '%1' is defined twice").arg(formula->name()));
	else if (_form->getControl(formula->name()))
		_form->addFormError(tr("A control and a formula have the same name '%1'").arg(formula->name()));
	else
		_formulas.append(formula);
}

bool RSyntax::parseRSyntaxOptions(Json::Value &options) const
{
	Log::log() << "Parse Syntax Options: " << options.toStyledString() << std::endl;
	if (!options.isObject())
	{
		addError("Wrong type of options!");
		return false;
	}

	Json::Value::Members members  = options.getMemberNames();
	for (const std::string& member : members)
	{
		QString syntaxName = tq(member),
				controlName = syntaxName;
		if (_rSyntaxToControlNameMap.contains(syntaxName))
		{
			controlName = _rSyntaxToControlNameMap[syntaxName];
			if (controlName != syntaxName)
			{
				options[fq(controlName)] = options[member];
				options.removeMember(member);
			}
		}

		JASPListControl* listControl = qobject_cast<JASPListControl*>(_form->getControl(controlName));
		if (listControl)
		{
			BoundControl* boundControl = listControl->boundControl();
			Json::Value defaultOption = boundControl != nullptr ? boundControl->defaultBoundValue() : Json::Value::null;
			const Json::Value& option = options[fq(controlName)];

			// For user-friendliness purpose, an option that should have a structure (list of strings, or list of lists of strings...),
			// can accept just a string. This may happen in 2 cases:
			// . a formula is used
			// . an array of strings contain only 1 element: this element is set without list.
			BoundControlTerms* boundControlTerms = dynamic_cast<BoundControlTerms*>(boundControl);

			if (option.isObject() && option.isMember("rhs") && boundControlTerms)
			{
				// When a formula is used, just parse it
				FormulaParser::ParsedTerms parsedTerms;
				QString error;

				if (!FormulaParser::parse(option["rhs"], false, parsedTerms, error))
				{
					addError(error);
					return false;
				}
				Json::Value newOption = boundControlTerms->addTermsToOption(Json::Value::null, parsedTerms.fixedTerms);
				options[fq(controlName)] = newOption;
			}
			else if (defaultOption.isArray() && option.isString())
			{
				// To make it easier, it is allowed to set a string when a Variables list has only 1 value: set back an array
				Json::Value newOption(Json::arrayValue);
				QString optionString = tq(option.asString());
				if (!optionString.isEmpty())
					newOption.append(fq(optionString));
				options[fq(controlName)] = newOption;
			}
		}
	}

	for (FormulaBase* formula : _formulas)
		formula->parseRSyntaxOptions(options);

	return !hasError();
}

void RSyntax::addError(const QString &msg) const
{
	_form->addControlError(_form->getControl(_form->rSyntaxControlName), msg);
}

bool RSyntax::hasError() const
{
	return _form->hasError();
}

QString RSyntax::transformJsonToR(const Json::Value &json)
{
	QString result;

	switch (json.type())
	{
		case Json::nullValue:
			result = "NULL";
		break;
		case Json::booleanValue:
			result = json.asBool() ? "TRUE" : "FALSE";
		break;
		case Json::stringValue:
			result = "\"" + tq(json.asString()) + "\"";
		break;
		case Json::intValue:
			result = QString::number(json.asInt());
		break;
		case Json::uintValue:
			result = QString::number(json.asUInt());
		break;
		case Json::realValue:
			result = QString::number(json.asDouble());
		break;
		case Json::arrayValue:
			if (json.size() == 0) result = "list()";
			else if (json.size() == 1 && (!json[uint(0)].isArray() && !json[uint(0)].isObject()))
				result = transformJsonToR(json[uint(0)]);
			else
			{
				bool first = true;
				int index = -1;
				result = "list(";
				for (const Json::Value& val : json)
				{
					index++;
					if (!first) result += ", ";
					result += transformJsonToR(val);
					first = false;
				}
				result += ")";
			}
		break;
		case Json::objectValue:
		{
			bool first = true;
			result = "list(";
			for (const std::string& member : json.getMemberNames())
			{
				const Json::Value& val = json.get(member, Json::Value::null);
				if (!first) result += ", ";
				result += tq(member) + " = " + transformJsonToR(val);
				first = false;
			}
			result += ")";
		}
	}

	return result;
}

bool RSyntax::_areTermsVariables(ListModel* model, const Terms& terms) const
{
	QStringList variables = model->requestInfo(VariableInfo::VariableNames).toStringList();

	for (const Term& term : terms)
		for (const QString& comp : term.components())
			if(!variables.contains(comp))
				return false;

	return true;
}

QString RSyntax::_transformInteractionTerms(ListModel* model) const
{
	const Terms& terms = model->terms();

	if (terms.size() == 0)	return "NULL";

	if (_areTermsVariables(model, terms))	return "~ " + FormulaSource::generateInteractionTerms(terms);

	QString result = "list(";
	bool first = true;
	for (const Term& term : terms)
	{
		if (!first) result += ", ";
		first = false;
		QStringList components = term.components();
		if (components.length() == 1)
			result += "\"" + components[0] + "\"";
		else
		{
			result += "list(";
			bool firstComponent = true;
			for (const QString& component : components)
			{
				if (!firstComponent) result += ", ";
				firstComponent = false;
				result += "\"" + component + "\"";
			}
			result += ")";
		}
	}

	result += ")";

	return result;
}

QString RSyntax::_analysisFullName() const
{
	return _form->module() + "::" + _form->name();
}

QString RSyntax::getRSyntaxFromControlName(JASPControl *control) const
{
	return getRSyntaxFromControlName(control->name());
}

QString	RSyntax::getRSyntaxFromControlName(const QString& name)	const
{
	return _controlNameToRSyntaxMap.value(name, name);
}

QString RSyntax::getControlNameFromRSyntax(const QString &name) const
{
	return _rSyntaxToControlNameMap.value(name, name);
}
