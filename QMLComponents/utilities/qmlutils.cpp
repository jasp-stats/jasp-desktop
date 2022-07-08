#include <QQmlIncubator>
#include <QQmlContext>
#include <QFileInfo>
#include "qmlutils.h"
#include "qutils.h"
#include "log.h"
#include "columnencoder.h"

QmlUtils::QmlUtils(QObject *parent) : QObject(parent)
{

}

QString QmlUtils::encodeAllColumnNames(const QString & str)
{
	return tq(ColumnEncoder::encodeAll(fq(str)));
}

QString QmlUtils::decodeAllColumnNames(const QString & str)
{
	return tq(ColumnEncoder::decodeAll(fq(str)));
}

QJSValue	QmlUtils::encodeJson(const QJSValue	& val, QQuickItem * caller)
{
	Json::Value v(fqj(val));
	ColumnEncoder::encodeJson(v);
	return tqj(v, caller);
}

QJSValue	QmlUtils::decodeJson(const QJSValue	& val, QQuickItem * caller)
{
	Json::Value v(fqj(val));
	ColumnEncoder::decodeJson(v);
	return tqj(v, caller);
}

//Turning QMLENGINE_DOES_ALL_THE_WORK on also works fine, but has slightly less transparent errormsgs so isn't recommended
//#define QMLENGINE_DOES_ALL_THE_WORK

QObject * instantiateQml(const QString & qmlTxt, const QUrl & url, const std::string & moduleName, const std::string & whatAmILoading, const std::string & filename, QQmlContext * ctxt)
{
	QObject * obj = nullptr;

#ifdef QMLENGINE_DOES_ALL_THE_WORK
	obj = MainWindow::singleton()->loadQmlData(qmlTxt, url);
#else
//	if(!ctxt)
//		ctxt = MainWindow::singleton()->giveRootQmlContext();

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

		throw std::runtime_error("There were errors loading " + filename + ":\n" + out.str());
	};

	errorLogger(qmlComp.isError(), qmlComp.errors());

	if(!qmlComp.isReady())
		throw std::runtime_error(whatAmILoading + " Component is not ready!");

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
		throw std::runtime_error(fq(filePath.toLocalFile()) + " is not a local file...");

	QFileInfo	qmlFileInfo(filePath.toLocalFile());

	if(!qmlFileInfo.exists())
		throw std::runtime_error(fq(qmlFileInfo.absoluteFilePath()) + " does not exist...");

	QString 	qmlTxt;
	QFile		qmlFile(qmlFileInfo.absoluteFilePath());

				qmlFile.open(QIODevice::ReadOnly);
	qmlTxt =	qmlFile.readAll();
				qmlFile.close();

	return instantiateQml(qmlTxt, filePath, moduleName, fq(qmlFileInfo.absoluteFilePath()), fq(qmlFileInfo.fileName()),  ctxt);

}
