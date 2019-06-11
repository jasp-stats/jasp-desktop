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

#ifndef QUTILS_H
#define QUTILS_H

#include <QString>
#include <list>
#include <string>
#include <vector>

enum Encryption { NoEncryption, SimpleCryptEncryption };

std::string fq(const QString &from);
QString tq(const std::string &from);
QStringList tql(const std::vector<std::string> &from);
std::vector<std::string> fromQstringToStdVector(const QString &input, const QString &delimetor);

QString stripFirstAndLastChar(QString in, const QString &strip);
QString getShortCutKey();
QString encrypt(const QString &input);
QString decrypt(const QString &input);
QString getSortableTimestamp();

#define GENERIC_SET_FUNCTION(WHAT_TO_SET, VARIABLE_TO_SET, EMIT_THIS, TYPE)	\
void set##WHAT_TO_SET(TYPE new##WHAT_TO_SET)								\
{																			\
	if(new##WHAT_TO_SET != VARIABLE_TO_SET)									\
	{																		\
		VARIABLE_TO_SET = new##WHAT_TO_SET;									\
		emit EMIT_THIS();													\
	}																		\
}



#endif // QUTILS_H
