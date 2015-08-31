
#include "fsbmexamples.h"

#include <QFile>
#include <QDir>

#include <QJsonDocument>
#include <QJsonParseError>
#include <QJsonArray>
#include <QJsonObject>

#include <QDebug>

#include "appdirs.h"

FSBMExamples::FSBMExamples(QObject *parent)
	: FSBModel(parent)
{

}

void FSBMExamples::refresh()
{
	QFile index(AppDirs::examples() + QDir::separator() + "index.json");

	if ( ! index.exists())
	{
		qDebug() << "BackStageForm::loadExamples();  index not found\n";
		return;
	}

	index.open(QFile::ReadOnly);
	if ( ! index.isOpen())
	{
		qDebug() << "BackStageForm::loadExamples();  index could not be opened\n";
		return;
	}

	QByteArray bytes = index.readAll();
	QJsonParseError error;

	QJsonDocument doc = QJsonDocument::fromJson(bytes, &error);

	if (error.error != QJsonParseError::NoError)
	{
		qDebug() << "BackStageForm::loadExamples();  JSON parse error : " << error.errorString() << "\n";
		return;
	}

	QJsonArray examples = doc.array();

	for (int i = examples.size() - 1; i >= 0; i--)
	{
		QJsonObject example = examples.at(i).toObject();
		QString path = AppDirs::examples() + QDir::separator() + example["path"].toString();
		QString name = example["name"].toString();
		QString description = example["description"].toString();

		_entries.append(createEntry(path, name, description, FSEntry::JASP));
	}
}

