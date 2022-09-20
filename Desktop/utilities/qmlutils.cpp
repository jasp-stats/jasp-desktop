#include "qmlutils.h"
#include "qutils.h"
#include "columnencoder.h"
#include "data/datasetpackage.h"

#ifdef linux

#include <QtGlobal>
#include <QStandardPaths>
#include "appinfo.h"
#include "log.h"

#endif

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


#ifdef linux
void QmlUtils::configureQMLCacheDir() {
	//set cache environment variable
	QDir cacheDir = QmlUtils::generateQMLCacheDir();
	bool set = qputenv("QML_DISK_CACHE_PATH", cacheDir.absolutePath().toLocal8Bit());
	if(!set) {
		throw std::runtime_error("Could not set qml cache directory in environment");
	}

	//delete stale caches
	QDir parent = cacheDir;
	parent.cdUp();
	QStringList staleCaches = parent.entryList(QStringList() << "qmlcache*", QDir::NoDot | QDir::NoDotDot | QDir::Dirs);
	for(auto& cacheName: staleCaches) {
		QDir staleCache = parent;
		staleCache.cd(cacheName);
		if(cacheDir.absolutePath() != staleCache.absolutePath()) {
			staleCache.removeRecursively();
		}
	}
	Log::log() << "QML cache directory: " + cacheDir.absolutePath() << std::endl;
}

QDir QmlUtils::generateQMLCacheDir() {
	QString commit = tq(AppInfo::gitCommit);
	QString basePath = qEnvironmentVariable("QML_DISK_CACHE_PATH", QStandardPaths::writableLocation(QStandardPaths::CacheLocation));
	QString path = basePath + "/qmlcache_" + commit;
	QDir cacheDir(path);
	if(!cacheDir.exists() && !cacheDir.mkpath("."))
		throw std::runtime_error("Could not create qml cache directory: " + fq(cacheDir.absolutePath()));
	cacheDir.refresh();
	return cacheDir;
}

#endif
