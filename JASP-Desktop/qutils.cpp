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

#include "qutils.h"

#include <QStringList>
#include <boost/foreach.hpp>

using namespace std;

std::string fq(const QString &from)
{
	QByteArray bytes = from.toUtf8();
	return std::string(bytes.constData(), bytes.length());
}

QString tq(const std::string &from)
{
	return QString::fromUtf8(from.c_str(), from.length());
}

QStringList tql(const std::vector<string> &from)
{
	(void)from;

	QStringList result;

	BOOST_FOREACH(const std::string &str, from)
	{
		(void)from;
		result.append(tq(str));
	}

	return result;
}

