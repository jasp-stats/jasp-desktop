#include "qmlutils.h"
#include "qutils.h"
#include "columnencoder.h"
#include "data/datasetpackage.h"

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
