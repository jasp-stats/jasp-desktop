#ifndef QMLUTILS_H
#define QMLUTILS_H

#include <QObject>
#include <QJSValue>
#include <QQuickItem>
#include <QDir>

/// Simply links through utilities for use in QML+JS
/// Only links to column encoding stuff for now though, in case it is needed.
class QmlUtils : public QObject
{
	Q_OBJECT
public:
	explicit QmlUtils(QObject *parent = nullptr);

#ifdef linux
// Functions for qml cache bug workaround on linux
public:
    static void configureQMLCacheDir();
private:
    static QDir generateQMLCacheDir();
#endif

public slots:
	QString		encodeAllColumnNames(	const QString	& str);
	QString		decodeAllColumnNames(	const QString	& str);

	QJSValue	encodeJson(				const QJSValue	& val, QQuickItem * caller);
	QJSValue	decodeJson(				const QJSValue	& val, QQuickItem * caller);

};

#endif // QMLUTILS_H
