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
#include "analysisqmldefines.h"
#include "log.h"

class JASPDoubleValidator : public QDoubleValidator
{
	Q_OBJECT

	Q_PROPERTY(QString inclusive READ inclusive	WRITE setInclusive	NOTIFY inclusiveChanged	)

public:
	JASPDoubleValidator (QObject* parent = nullptr) : QDoubleValidator(parent) {}

	QValidator::State	validate(QString& s, int& pos) const override;

	Q_INVOKABLE QString	validationMessage(const QString& fieldName);

	QString inclusive() { return _capitalize(qmlInclusiveTypeToQString(_inclusive), false); }
	void setInclusive(QString inclusive)
	{
		try { _inclusive = qmlInclusiveTypeFromQString(_capitalize(inclusive, true));	}
		catch (...) { Log::log() << "Wrong Inclusive type " << inclusive.toStdString() << std::flush; }
	}

signals:
	void inclusiveChanged();

protected:
	qmlInclusiveType	_inclusive = qmlInclusiveType::Yes;

private:
	QString _capitalize(const QString& str, bool toCapitalize = true);
	bool	_isInf(double value);
};

#endif // JASPDOUBLEVALIDATOR_H
