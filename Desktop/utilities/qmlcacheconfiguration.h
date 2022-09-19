#ifndef QMLCACHECONFIGURATION_H
#define QMLCACHECONFIGURATION_H

#ifdef linux
#include <QDir>

class QMLCacheConfiguration
{
public:
    static void configureQMLCacheDir();
private:
    static QDir generateQMLCacheDir();
};


#endif

#endif // QMLCACHECONFIGURATION_H
