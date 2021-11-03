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

#ifndef JASPDOUBLEVALIDATOR_H
#define JASPDOUBLEVALIDATOR_H

#include <QDoubleValidator>
#include "analysis/jaspcontrol.h"
#include "log.h"

/// 
/// This is used in QML to verify whether entered numbers in `DoubleField` are correct accordin to the desired settings.
///
class JASPDoubleValidator : public QDoubleValidator
{
	Q_OBJECT

	Q_PROPERTY(JASPControl::Inclusive inclusive READ inclusive	WRITE setInclusive	NOTIFY inclusiveChanged	)

public:
	JASPDoubleValidator (QObject* parent = nullptr) : QDoubleValidator(parent) {}

	QValidator::State	validate(QString& s, int& pos) const override;

	Q_INVOKABLE QString	validationMessage(const QString& fieldName);

	GENERIC_SET_FUNCTION(Inclusive, _inclusive, inclusiveChanged, JASPControl::Inclusive)

	JASPControl::Inclusive inclusive() { return _inclusive; }

signals:
	void inclusiveChanged();

protected:
	JASPControl::Inclusive	_inclusive = JASPControl::Inclusive::MinMax;

private:
	bool	_isInf(double value);
};

#endif // JASPDOUBLEVALIDATOR_H
