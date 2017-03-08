//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef LRNAM_H
#define LRNAM_H

#include <QNetworkAccessManager>

#include "common.h"

class LRNAM : public QNetworkAccessManager
{
	Q_OBJECT

public:
	LRNAM(const QString &baseResourceDirectory, QObject *parent = 0);

private:
	QString _baseResourceDirectory;

protected:
	QNetworkReply *createRequest(Operation op, const QNetworkRequest &request, QIODevice *outgoingData) OVERRIDE;

};

#endif // LRNAM_H
