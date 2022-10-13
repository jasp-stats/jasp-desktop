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

#ifndef FORMULABASE_H
#define FORMULABASE_H

#include <QQuickItem>
#include "utilities/qutils.h"
#include "formulaparser.h"

class FormulaSource;
class RSyntax;
class AnalysisForm;
class ListModel;

class FormulaBase : public QQuickItem
{
	Q_OBJECT

	Q_PROPERTY( QVariant	userMustSpecify		READ userMustSpecify		WRITE setUserMustSpecify		NOTIFY userMustSpecifyChanged		)
	Q_PROPERTY( QVariant	lhs					READ lhs					WRITE setLhs					NOTIFY lhsChanged					)
	Q_PROPERTY( QVariant	rhs					READ rhs					WRITE setRhs					NOTIFY rhsChanged					)
	Q_PROPERTY( QString		name				READ name					WRITE setName					NOTIFY nameChanged			)

public:
	FormulaBase(QQuickItem *parent = nullptr);

	void				setUp();
	QVariant			userMustSpecify()													const	{ return _userMustSpecify;	}
	QVariant			lhs()																const	{ return _lhs;				}
	QVariant			rhs()																const	{ return _rhs;				}
	QString				name()																const	{ return _name;				}

	QString				toString()															const;
	bool				parseRSyntaxOptions(Json::Value &options)							const;
	RSyntax*			rSyntax()															const	{ return _rSyntax;			}
	AnalysisForm	*	form()																const;
	QStringList			modelSources()														const;
	QStringList			sourcesThatMustBeSpecified()										const;
	QStringList			extraOptions(bool useOptionName = true, bool onlyFormula = false)	const;

	ListModel*			getModel(const QString& name)										const;

signals:
	void userMustSpecifyChanged();
	void lhsChanged();
	void rhsChanged();
	void somethingChanged();
	void nameChanged();

public slots:
	GENERIC_SET_FUNCTION(UserMustSpecify		, _userMustSpecify		, userMustSpecifyChanged		, QVariant		)
	GENERIC_SET_FUNCTION(Lhs					, _lhs					, lhsChanged					, QVariant		)
	GENERIC_SET_FUNCTION(Rhs					, _rhs					, rhsChanged					, QVariant		)
	GENERIC_SET_FUNCTION(Name					, _name					, nameChanged					, QString		)

protected:
	void componentComplete() override;

private:
	QStringList				_getSourceList(const QVariant& var)																								const;
	bool					_parseFormulaSources(const QVector<FormulaSource*>& formulaSources, FormulaParser::ParsedTerms& formula, Json::Value& options)	const;

	RSyntax*				_rSyntax			= nullptr;
	QVariant				_userMustSpecify,
							_lhs,
							_rhs;
	QString					_name			= "formula";
	QVector<FormulaSource*>	_leftFormulaSources,
							_rightFormulaSources;
};

#endif // FORMULABASE_H
