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

#ifndef RSYNTAX_H
#define RSYNTAX_H

#include <QQuickItem>
#include "formulabase.h"

class AnalysisForm;
class JASPControl;
class Terms;

class RSyntax : public QObject
{
	Q_OBJECT

public:
	RSyntax(AnalysisForm *form);

	AnalysisForm*					form()											const	{ return _form;					}
	QVariantList					controlNameToRSyntaxMap()						const;
	bool							setControlNameToRSyntaxMap(const QVariantList& conv);

	QString							generateSyntax()								const;
	QString							generateWrapper()								const;
	QString							getRSyntaxFromControlName(JASPControl* control)	const;
	QString							getRSyntaxFromControlName(const QString& name)	const;
	QString							getControlNameFromRSyntax(const QString& name)	const;
	void							setUp();
	void							addFormula(FormulaBase* formula);
	FormulaBase*					getFormula(const QString& name)					const;
	bool							parseRSyntaxOptions(Json::Value& options)		const;
	void							addError(const QString& msg)					const;
	bool							hasError()										const;

	static QString					FunctionOptionIndent,
									FunctionLineIndent;
	static QString					transformJsonToR(const Json::Value& json, const Json::Value& comparedValue = Json::nullValue);
	static QString					transformInteractionTerms(const Terms& terms, bool useFormula = true);

signals:
	void							somethingChanged();


private:

	QString							_analysisFullName(bool wrapper = false)		const;

	AnalysisForm*					_form							= nullptr;
	QVector<FormulaBase*>			_formulas;
	QMap<QString, QString>			_controlNameToRSyntaxMap;
	QMap<QString, QString>			_rSyntaxToControlNameMap;
};

#endif // RSYNTAX_H
