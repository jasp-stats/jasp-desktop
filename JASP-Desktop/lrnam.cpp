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

#include "lrnam.h"

#include <QNetworkRequest>

#include "lrnamreply.h"


LRNAM::LRNAM(const QString &baseResourceDirectory, QObject *parent)
	: QNetworkAccessManager(parent)
{
	_baseResourceDirectory = baseResourceDirectory;
}

QNetworkReply *LRNAM::createRequest(QNetworkAccessManager::Operation op, const QNetworkRequest &request, QIODevice *outgoingData)
{	
	QUrl url = request.url();

	QString path = url.path();
	if (op == QNetworkAccessManager::GetOperation && url.isLocalFile())
	{
			path = url.toLocalFile();
			return new LRNAMReply(path);
	}
	else if (path.startsWith("/core/resources/"))
	{
		path = _baseResourceDirectory + path.mid(5);
		return new LRNAMReply(path);
	}
	else
	{
		return QNetworkAccessManager::createRequest(op, request, outgoingData);
	}
}

