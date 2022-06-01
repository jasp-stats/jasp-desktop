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
#include <QQmlEngine>
#include <QQmlContext>
#include <QQmlIncubator>
#include <QStringList>
#include <QDateTime>
#include "appinfo.h"
#include "simplecrypt.h"
#include "utils.h"
#include "mainwindow.h"
#include "log.h"

using namespace std;

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
	long long key = AppInfo::getSimpleCryptKey();
	SimpleCrypt crypto(key); //some random number
	
	return crypto.encryptToString(input);	
}

QString decrypt(const QString &input)
{	
	long long key = AppInfo::getSimpleCryptKey();
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

	return QVector<QString>{out.begin(), out.end()};
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

Json::Value fqj(const QJSValue & jsVal)
{
	if(jsVal.isNull())		return Json::nullValue;
	if(jsVal.isBool())		return jsVal.toBool();
	if(jsVal.isNumber())	return jsVal.toNumber();
	if(jsVal.isString())	return fq(jsVal.toString());

	if(jsVal.isArray())
	{
		Json::Value  json	= Json::arrayValue;
		const size_t length = jsVal.property("length").toUInt();

		for(size_t i=0; i<length; i++)
			json.append(fqj(jsVal.property(i)));

		return json;
	}

	if(jsVal.isObject() && !(jsVal.isCallable() || jsVal.isQObject() || jsVal.isVariant()))
	{
		Json::Value json = Json::objectValue;
		QJSValueIterator it(jsVal);

		while(it.hasNext())
		{
			it.next();
			json[fq(it.name())] = fqj(it.value());
		}

		return json;
	}

	return Json::nullValue; //Qt supports more types in QJSValue than Json::Value does. So make it null
}

QJSValue tqj(const Json::Value & json, const QQuickItem * qItem)
{
	switch(json.type())
	{
	default:
	case Json::nullValue:		return QJSValue(QJSValue::SpecialValue::NullValue);
	case Json::stringValue:		return QJSValue(tq(json.asString()));
	case Json::intValue:
	case Json::uintValue:
	case Json::realValue:		return QJSValue(json.asDouble());
	case Json::booleanValue:	return QJSValue(json.asBool());
	case Json::arrayValue:
	{
		QJSValue array = QQmlEngine::contextForObject(qItem)->engine()->newArray(json.size());

		for(auto i=0; i<json.size(); i++)
			array.setProperty(i, tqj(json[i], qItem));

		return array;
	}
	case Json::objectValue:
	{
		QJSValue obj = QQmlEngine::contextForObject(qItem)->engine()->newObject();

		for(const std::string & member : json.getMemberNames())
			obj.setProperty(tq(member), tqj(json[member], qItem));

		return obj;
	}
	}

	return QJSValue(QJSValue::SpecialValue::NullValue); //In case some compiler doesnt get that I covered everything already.
}

QString QJSErrorToString(QJSValue::ErrorType errorType)
{
	switch(errorType)
	{
	default:
	case QJSValue::ErrorType::GenericError:		return "A non-specific error...";
	case QJSValue::ErrorType::RangeError:		return "A value did not match the expected set or range.";
	case QJSValue::ErrorType::ReferenceError:	return "A non-existing variable referenced.";
	case QJSValue::ErrorType::SyntaxError:		return "An invalid token or sequence of tokens was encountered that does not conform with the syntax of the language.";
	case QJSValue::ErrorType::TypeError:		return "An operand or argument is incompatible with the type expected.";
	case QJSValue::ErrorType::URIError:			return "A URI handling function was used incorrectly or the URI provided is malformed.";
	}
	
	return "Could not determine error from type.";
}

QString shortenWinPaths(QString in)
{
#ifdef _WIN32
	return QString::fromStdWString(Utils::getShortPathWin(in.toStdWString()));
#else
	return in;
#endif
}


//Turning QMLENGINE_DOES_ALL_THE_WORK on also works fine, but has slightly less transparent errormsgs so isn't recommended
//#define QMLENGINE_DOES_ALL_THE_WORK

QObject * instantiateQml(const QString & qmlTxt, const QUrl & url, const std::string & moduleName, const std::string & whatAmILoading, const std::string & filename, QQmlContext * ctxt)
{
	QObject * obj = nullptr;

#ifdef QMLENGINE_DOES_ALL_THE_WORK
	obj = MainWindow::singleton()->loadQmlData(qmlTxt, url);
#else
	if(!ctxt)
		ctxt = MainWindow::singleton()->giveRootQmlContext();

	QQmlComponent qmlComp(ctxt->engine());

	//Log::log() << "Setting url to '" << url.toString() << "' for Description.qml.\n" << std::endl;// data: '" << descriptionTxt << "'\n"<< std::endl;

	qmlComp.setData(qmlTxt.toUtf8(), url);

	if(qmlComp.isLoading())
		Log::log() << whatAmILoading << " for module " << moduleName << " is still loading, make sure you load a local file and that Windows doesn't mess this up for you..." << std::endl;


	auto errorLogger =[&](bool isError, QList<QQmlError> errors)
	{
		if(!isError) return;

		std::stringstream out;

		out << "Loading " << filename << " for module " << moduleName << " had errors:\n";

		for(const QQmlError & error : errors)
			out << error.toString() << "\n";

		Log::log() << out.str() << std::flush;

		throw Modules::ModuleException(moduleName, "There were errors loading " + filename + ":\n" + out.str());
	};

	errorLogger(qmlComp.isError(), qmlComp.errors());

	if(!qmlComp.isReady())
		throw Modules::ModuleException(moduleName, whatAmILoading + " Component is not ready!");

	QQmlIncubator localIncubator(QQmlIncubator::Synchronous);


	qmlComp.create(localIncubator);

	errorLogger(localIncubator.isError(), localIncubator.errors());

	obj = localIncubator.object();

#endif

	return obj;
}

QObject * instantiateQml(const QUrl & filePath, const std::string & moduleName, QQmlContext * ctxt)
{
	if(!filePath.isLocalFile())
		throw Modules::ModuleException(moduleName, fq(filePath.toLocalFile()) + " is not a local file...");

	QFileInfo	qmlFileInfo(filePath.toLocalFile());

	if(!qmlFileInfo.exists())
		throw Modules::ModuleException(moduleName, fq(qmlFileInfo.absoluteFilePath()) + " does not exist...");

	QString 	qmlTxt;
	QFile		qmlFile(qmlFileInfo.absoluteFilePath());
	
				qmlFile.open(QIODevice::ReadOnly);
	qmlTxt =	qmlFile.readAll();
				qmlFile.close();

	return instantiateQml(qmlTxt, filePath, moduleName, fq(qmlFileInfo.absoluteFilePath()), fq(qmlFileInfo.fileName()),  ctxt);

}
