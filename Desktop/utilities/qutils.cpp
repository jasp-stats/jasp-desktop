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

#include "utilities/qutils.h"

#include <QStringList>

#include <QDateTime>
#include "simplecrypt.h"
#include "simplecryptkey.h"

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
	QStringList result;

	for(const std::string &str : from)
		result.append(tq(str));

	return result;
}

vector<string> fromQstringToStdVector(const QString &input, const QString &delimeter)
{
	QStringList list;
	vector<string> result;
	list = input.split(delimeter);
	for (const QString & itm : list)
		result.push_back(stripFirstAndLastChar(itm,"\"").toStdString());
	
	return result;
}

QString stripFirstAndLastChar(QString result, const QString &strip)
{
	if (result.left(1) == strip) result.remove(0,1);
	if (result.right(1) == strip) result.remove(result.length()-1,1);
	return result;	
}

QString getShortCutKey()
{
#ifdef __APPLE__
		QString shortCutKey = "\u2318";
#else
		QString shortCutKey = "Ctrl";
#endif	
		return shortCutKey;
}

QString encrypt(const QString &input)
{
	long long key = SIMPLECRYPTKEY;
	SimpleCrypt crypto(key); //some random number
	
	return crypto.encryptToString(input);	
}

QString decrypt(const QString &input)
{	
	long long key = SIMPLECRYPTKEY;
	SimpleCrypt crypto(key); //some random number
	
	return crypto.decryptToString(input);
}

QString getSortableTimestamp()
{
	return QDateTime::currentDateTime().toString("yyyy-MM-dd hh_mm_ss"); //This order gets an easy alphanumeric sort by default and sadly enough the character : is not allowed on unix/macx
}

QVector<QString> tq(const std::vector<std::string> & vec)
{
	std::vector<QString> out;
	out.reserve(vec.size());
	for(const std::string & s : vec)
		out.push_back(tq(s));

	return QVector<QString>(out.begin(), out.end());
}

std::vector<std::string> fq(const QVector<QString> & vec)
{
	std::vector<std::string> out;
	out.reserve(static_cast<size_t>(vec.size()));

	for(const QString & s : vec)
		out.push_back(fq(s));

	return out;

}

std::map<std::string, std::string>	fq(const QMap<QString, QString> & map)
{
	std::map<std::string, std::string>	out;

	for(const auto & keyval : map.toStdMap())
		out[fq(keyval.first)] = fq(keyval.second);

	return out;
}

QMap<QString, QString> tq(const std::map<std::string, std::string> & map)
{
	QMap<QString, QString> out;

	for(const auto & keyval : map)
		out[tq(keyval.first)] = tq(keyval.second);

	return out;
}


const char * QProcessErrorToString(QProcess::ProcessError error)
{
	switch(error)
	{
	case QProcess::ProcessError::Crashed:		return "Crashed";
	case QProcess::ProcessError::Timedout:		return "Timedout";
	case QProcess::ProcessError::ReadError:		return "ReadError";
	case QProcess::ProcessError::WriteError:	return "WriteError";
	case QProcess::ProcessError::UnknownError:	return "UnknownError";
	case QProcess::ProcessError::FailedToStart:	return "FailedToStart";
	};
	return "???";
}

bool pathIsSafeForR(const QString & checkThis)
{
	return checkThis.toLocal8Bit() == checkThis.toUtf8();
}
