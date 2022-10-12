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

#ifndef FORMULASOURCE_H
#define FORMULASOURCE_H

#include <QQuickItem>
#include "formulaparser.h"
#include <json/json.h>
#include "models/listmodel.h"

class FormulaBase;
class ListModelAssignedInterface;
class Terms;
class ComponentsListBase;

class FormulaSource : public QObject
{
	Q_OBJECT

public:
	struct ExtraOption
	{
		QString optionName;
		bool	useFormula = true;

		ExtraOption(const QString& _optionName, bool _useFormula = true)
			: optionName(_optionName), useFormula(_useFormula) {}
		ExtraOption(const QString& controlName, const QVariant& var);
		ExtraOption() {}
	};

	struct RandomEffects
	{
		QString		name,
					fixedEffectsSource	= "fixedEffects",
					variablesControl	= "randomComponents",
					checkControl		= "randomSlopes",
					correlationControl	= "correlations",
					variablesKey		= "value";
		ListModel*	fixedEffectsModel = nullptr;
		ComponentsListBase* componentsList = nullptr; // A RandomEffects can be made only from a ComponentsList item

		RandomEffects(const QVariant& var);
		RandomEffects() {}

		bool isEmpty() const { return name.isEmpty(); }
	};

	FormulaSource(FormulaBase *formula, const QVariant& var);

	static QVector<FormulaSource*>	makeFormulaSources(FormulaBase* formula, const QVariant& var);
	static QString					generateInteractionTerms(const Terms& terms);

	const QString&					sourceName()																					const	{ return _sourceName;	}
	QStringList						modelSources()																					const;
	QString							toString()																						const;
	QMap<QString, QString>			additionalOptionStrings()																		const;
	bool							isEmpty()																						const;
	ListModel*						model()																							const	{ return _model;	}
	FormulaParser::ParsedTerms		fillOptionsWithParsedTerms(const FormulaParser::ParsedTerms& parsedTerms, Json::Value& options)	const;
	bool							hasRandomEffects()																				const	{ return !_randomEffects.isEmpty(); }
	QStringList						extraOptions(bool useOptionName = true, bool onlyFormula = false)								const;

protected:
	void							_addError(const QString& error)																	const;
	std::string						_controlToOptionName(const QString& name)														const;
	QString							_generateRandomEffectsTerms(const Terms& terms)													const;
	QString							_generateSimpleTerms(const Terms& terms)														const;
	Terms							_onlyTrueTerms(const QString& controlName, const Terms& terms)									const;
	bool							_areTermsInOptions(ListModelAssignedInterface* model, const Json::Value& options, Terms& terms)	const;
	void							_addTermsToOptions(ListModelAssignedInterface* model, Json::Value& options, const Terms& terms)	const;
	FormulaParser::ParsedTerms		_fillOptionsWithParsedTerms(ListModel* model, const FormulaParser::ParsedTerms &parsedTerms, Json::Value &options, QMap<QString, Terms>* termsMap = nullptr)	const;
	FormulaParser::ParsedTerms		_fillOptionsWithConditionalParsedTerms(const FormulaParser::ParsedTerms& parsedTerms, Json::Value& options)														const;
	ListModel::RowControlsValues	_getTermsFromExtraOptions(const Json::Value& options)											const;
	FormulaBase*						_formula			= nullptr;
	ListModel*						_model				= nullptr;
	QString							_sourceName;
	QMap<QString, ExtraOption>		_extraOptions;
	RandomEffects					_randomEffects;
};

#endif // FORMULASOURCE_H
