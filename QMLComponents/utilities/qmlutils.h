#ifndef QMLUTILS_H
#define QMLUTILS_H

#include <QObject>
#include <QJSValue>
#include <QQuickItem>

/// Simply links through utilities for use in QML+JS
/// Only links to column encoding stuff for now though, in case it is needed.
class QmlUtils : public QObject
{
	Q_OBJECT
public:
	explicit QmlUtils(QObject *parent = nullptr);

public slots:
	QString		encodeAllColumnNames(	const QString	& str);
	QString		decodeAllColumnNames(	const QString	& str);

	QJSValue	encodeJson(				const QJSValue	& val, QQuickItem * caller);
	QJSValue	decodeJson(				const QJSValue	& val, QQuickItem * caller);
};

QObject * instantiateQml(const QUrl & filePath, const std::string & moduleName, QQmlContext * ctxt = nullptr);
QObject * instantiateQml
(const QString & qmlTxt, const QUrl & url, const std::string & moduleName, const std::string & whatAmILoading, const std::string & filename, QQmlContext * ctxt = nullptr);


#endif // QMLUTILS_H
