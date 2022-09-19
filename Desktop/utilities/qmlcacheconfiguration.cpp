#ifdef linux

#include "qmlcacheconfiguration.h"
#include <QtGlobal>
#include <QStandardPaths>
#include "appinfo.h"
#include "utilities/qutils.h"
#include "log.h"


void QMLCacheConfiguration::configureQMLCacheDir() {
    //set cache environment variable
    QDir cacheDir = QMLCacheConfiguration::generateQMLCacheDir();
    bool set = qputenv("QML_DISK_CACHE_PATH", cacheDir.absolutePath().toLocal8Bit());
    if(!set) {
        throw std::runtime_error("Could not set qml cache directory in environment");
    }

    //delete stale caches
    QDir parent = cacheDir;
    parent.cdUp();
    QStringList staleCaches = parent.entryList(QStringList() << "qmlcache_*", QDir::NoDot | QDir::NoDotDot | QDir::Dirs);
    for(auto& cacheName: staleCaches) {
        QDir staleCache = parent;
        staleCache.cd(cacheName);
        if(cacheDir.absolutePath() != staleCache.absolutePath()) {
           staleCache.removeRecursively();
        }
    }
    Log::log() << "QML cache directory: " + cacheDir.absolutePath() << std::endl;
}

QDir QMLCacheConfiguration::generateQMLCacheDir() {
    QString commit = tq(AppInfo::gitCommit);
    QString basePath = qEnvironmentVariable("QML_DISK_CACHE_PATH", QStandardPaths::writableLocation(QStandardPaths::CacheLocation));
    QString path = basePath + "/qmlcache_" + commit;
    QDir cacheDir(path);
    if(!cacheDir.exists()) {
        bool succes = cacheDir.mkpath(".");
        if(!succes) {
            throw std::runtime_error("Could not create qml cache directory: " + fq(cacheDir.absolutePath()));
        }
    }
    cacheDir.refresh();
    return cacheDir;
}


#endif
